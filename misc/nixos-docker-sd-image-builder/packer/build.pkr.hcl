# - NixOS SD image builder AWS configuration
# This will spin up a `a1.2xlarge` instance and build an SD image for NixOS on it using the
# contents of the cloned repository. It will also automatically download the image from the
# remote instance.
# I recommend to enable compression in the main configuration file to minimize downloading
# files.
#
# NOTE: This requires at least Packer 1.5.0.

variable "region" {
  default = "us-east-2"
}

variable "availability_zone" {
  # note: us-east-2c (used by default) does not have ARM spot instances
  default = "us-east-2a"
}

source "amazon-ebs" "nixos_sd_image_builder" {
  ami_name            = "nixos_sd_image_builder"
  region              = var.region
  availability_zone   = var.availability_zone
  # This instance has 8 cores and 16 GiB of RAM. It is pretty cheap with Spot and builds the image
  # in about 5 minutes.
  spot_instance_types = ["a1.2xlarge"]
  spot_price          = "auto"

  source_ami_filter {
    filters = {
      name = "debian-10-arm64-*"
    }

    most_recent = true

    owners = ["136693071363"] # source: https://wiki.debian.org/Cloud/AmazonEC2Image/Buster
  }

  # The default volume size of 8 GiB is too small. Use 16.
  launch_block_device_mappings {
    device_name           = "/dev/xvda"
    volume_type           = "gp2"
    volume_size           = "16"
    delete_on_termination = true
  }

  ssh_username = "admin"
}

build {
  sources = ["source.amazon-ebs.nixos_sd_image_builder"]

  # Copies Docker stuff.
  provisioner "file" {
    source      = "../docker"
    destination = "./"
  }

  # Copies the run script.
  # NOTE: this won't be actually executed, as the last `shell` step copies the script in a
  # temporary directory, but we're copying anyway so that the build can be re-executed if needed.
  provisioner "file" {
    source      = "../run.sh"
    destination = "./run.sh"
  }

  # Copies the configuration file(s).
  provisioner "file" {
    source      = "../config"
    destination = "./"
  }

  # Installs dependencies and gets the run script ready for other re-executions.
  provisioner "shell" {
    inline = [
      "chmod +x run.sh",
      "sudo apt-get update -y",
      "sudo apt-get install -y docker.io docker-compose"
    ]
  }

  # Builds the image.
  provisioner "shell" {
    script = "../run.sh"
  }

  # Downloads the image.
  provisioner "file" {
    source      = "./nixos*"
    destination = "./"
    direction   = "download"
  }

  provisioner "shell-local" {
    inline = [
      "echo 'Image *successfully* built and downloaded as' nixos*",
      "echo 'To prevent Packer from creating an AMI, this will now produce a failed exit code.'",
      "echo ' to learn why, see: https://github.com/hashicorp/packer/pull/4681'",
      "echo 'NOTE: the process was successful!'",
      "exit 1"
    ]
  }
}
