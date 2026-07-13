programs.agent-skills.skills.explicit = {
  # Simple case: packages with automatic command rewriting (default)
  # All occurrences of "jq" in SKILL.md are rewritten to "./jq"
  my-tool = {
    from = "my-source";
    path = "some-skill";
    packages = [ pkgs.jq pkgs.curl ];
  };

  # Disable automatic rewriting if you want bare command names
  my-tool-no-rewrite = {
    from = "my-source";
    path = "some-skill";
    packages = [ pkgs.jq ];
    rewriteCommands = false;
  };

  # Full customisation with transform (rewriteCommands applies before transform)
  my-skill = {
    from = "my-source";
    path = "some-skill";
    packages = [ pkgs.jq pkgs.curl ];
    transform = { original, dependencies }: ''
      # Custom Header

      ${dependencies}

      ${original}

      # See Also
      - https://example.com
    '';
  };
};
