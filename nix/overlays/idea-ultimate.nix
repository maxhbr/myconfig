self: super: {
  idea.idea-ultimate = super.idea.idea-ultimate.overrideAttrs (oldAttrs: rec {
    name = "idea-ultimate-${version}";
    version = "2017.1.4";
    src = self.fetchurl {
      url = "https://download.jetbrains.com/idea/ideaIU-${version}.tar.gz";
      sha256 = "0a93fba480pvdh6x263fm7rb9w728smgx65nbvkkbdqngjmnanyx";
    };
  });
}
