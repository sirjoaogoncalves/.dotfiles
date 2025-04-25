👻 Ghost Emacs Configuration
============================

Overview
--------

This is a comprehensive Emacs configuration designed to provide a modern, efficient, and feature-rich editing experience. The configuration is modular, organized, and packed with powerful tools for developers, writers, and power users.

Key Packages
------------

### Completion & Navigation

*   **Vertico**: Vertical completion UI
    
*   **Marginalia**: Annotations in minibuffer
    
*   **Consult**: Enhanced search and navigation
    
*   **Orderless**: Flexible matching
    
*   **Corfu**: In-buffer completion
    
*   **Cape**: Completion extensions
    

### Development

*   **Projectile**: Project management
    
*   **LSP Mode**: Language Server Protocol support
    
*   **Flycheck**: Syntax checking
    
*   **YASnippet**: Code templates
    
*   **Format All**: Automatic code formatting
    

### Version Control

*   **Magit**: Git client
    
*   **Git Gutter**: Version control indicators
    
*   **Blamer**: Git blame information
    

### Productivity

*   **Org Mode**: Advanced task management and note-taking
    
*   **Org Roam**: Personal knowledge management
    
*   **Treemacs**: Project explorer
    
*   **Dashboard**: Custom startup screen
    

### UI & Appearance

*   **Doom Themes**: Modern color themes
    
*   **Doom Modeline**: Elegant modeline
    
*   **All-the-icons**: Icon support
    
*   **Rainbow Delimiters**: Colorful bracket matching
    

### Editing & Navigation

*   **Evil Mode**: Vim-like modal editing
    
*   **Ace Window**: Quick window navigation
    
*   **Popper**: Popup window management
    

### Performance

*   **GCMH**: Garbage collection optimization
    
*   **Native Compilation**: Faster Emacs startup
    

### Remote & Terminal

*   **TRAMP**: Remote file editing
    
*   **vterm**: Terminal emulation
    

Leader Key Bindings
-------------------

The configuration uses SPC (Space) as the leader key, providing a consistent and intuitive interface.

### Global Bindings

*   SPC f: File operations
    
*   SPC b: Buffer management
    
*   SPC w: Window management
    
*   SPC p: Project operations
    
*   SPC g: Git operations
    
*   SPC o: Org mode
    
*   SPC a: AI tools (Minuet)
    
*   SPC r: Remote operations
    
*   SPC t: Treemacs and tabs
    
*   SPC l: Layouts and LSP
    
*   SPC c: Completion
    
*   SPC d: Dashboard
    
*   SPC h: Help and documentation
    

### Example Bindings

*   SPC ff: Find file
    
*   SPC bb: Switch buffer
    
*   SPC wv: Split window vertically
    
*   SPC gs: Git status
    
*   SPC oa: Org agenda
    
*   SPC am: Show Minuet suggestion
    
*   SPC rf: Edit remote file
    

Installation
------------

1.  Clone this repository to ~/.emacs.d/
    
2.  Install Emacs 27+ with native compilation support
    
3.  Install required fonts (All-the-icons recommended)
    
4.  Run Emacs and let it install packages automatically
    

Folder Structure
----------------

Plain textANTLR4BashCC#CSSCoffeeScriptCMakeDartDjangoDockerEJSErlangGitGoGraphQLGroovyHTMLJavaJavaScriptJSONJSXKotlinLaTeXLessLuaMakefileMarkdownMATLABMarkupObjective-CPerlPHPPowerShell.propertiesProtocol BuffersPythonRRubySass (Sass)Sass (Scss)SchemeSQLShellSwiftSVGTSXTypeScriptWebAssemblyYAMLXML`   .emacs.d/  │  ├── core/  │   ├── defaults.el  │   ├── keybindings.el  │   ├── packages.el  │   └── ui.el  │  ├── init.el  │  └── modules/      ├── ai.el      ├── completion.el      ├── dashboard-config.el      ├── development.el      ├── dired-config.el      ├── evil-config.el      ├── git.el      ├── ide-config.el      ├── org-config.el      ├── performance.el      ├── terminal.el      ├── tramp-config.el      └── window-config.el   `

Contributing
------------

Feel free to open issues or submit pull requests to improve this configuration!

License
-------

MIT

