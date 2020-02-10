FROM base/archlinux
MAINTAINER Maximilian Huber <maximilian.huber@tngtech.com>

RUN set -x \
 && pacman-db-upgrade \
 && pacman -Syy --noconfirm \
 && pacman -S --noconfirm wget
RUN wget https://aur.archlinux.org/cgit/aur.git/snapshot/mutt-sidebar.tar.gz

COPY docker-entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["bash"]
