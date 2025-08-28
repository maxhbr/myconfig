{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig;
in
{
  config = lib.mkIf (cfg.email.enable && (builtins.elem "alot" cfg.email.clients)) {
    home-manager.sharedModules = [
      (
        {
          config,
          lib,
          pkgs,
          ...
        }:

        {
          programs.alot = {
            enable = true;
            # bindings = {
            #   global = {
            #     h = "bclose";
            #     n = "move down";
            #     e = "move up";
            #     i = "select";
            #     o = "refresh";
            #     u = "toggletags unread";
            #     "/" = "prompt 'search '";
            #   };
            #   search = {
            #     i = "select; fold *; unfold tag:unread; move first";
            #     U = "search tag:unread; move last";
            #   };
            #   thread = {
            #     h = "bclose; refresh";
            #     n = "move next";
            #     e = "move previous";
            #     "' '" = "move page down";
            #     o = "fold; untag unread; move next unfolded";
            #     y = "pipeto 'urlscan -dW 2>/dev/null'";
            #     r = "reply --all";
            #     R = "reply";
            #     u = "fold; untag unread; move next tag:unread";
            #   };
            # };
            settings = {
              attachment_prefix = "~/Downloads/";
              #auto_remove_unread = true;
            };
            # hooks = ''
            #   import alot
            #   def pre_buffer_focus(ui, dbm, buf):
            #       if buf.modename == 'search':
            #           buf.rebuild()
            #   def pre_buffer_open(ui, dbm, buf):
            #       current = ui.current_buffer
            #       if isinstance(current, alot.buffers.SearchBuffer):
            #           current.focused_thread = current.get_selected_thread()
            #   def post_buffer_focus(ui, dbm, buf, success):
            #       if success and hasattr(buf, "focused_thread"):
            #           if buf.focused_thread is not None:
            #               tid = buf.focused_thread.get_thread_id()
            #               flag = False
            #               for pos, tlw in enumerate(buf.threadlist.get_lines()):
            #                   flag = True
            #                   if tlw.get_thread().get_thread_id() == tid:
            #                       break
            #               if flag:
            #                   buf.body.set_focus(pos)
            # '';
            tags = {
              attachment.translated = "a";
              encrypted.translated = "e";
              inbox.translated = "i";
              replied.translated = "r";
              signed.translated = "s";
              unread.translated = "U";
            };
          };
        }
      )
    ];
  };
}
