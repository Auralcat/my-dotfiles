" ============================================================================
" MODULAR VIM CONFIGURATION
" ============================================================================
" Main vimrc file that sources all configuration modules
" Organization: Basic → Plugins → Keymaps → Completion → Writing → Programming
"
" Configuration Structure:
"   ~/.vimrc (this file)
"   ~/.vim/config/
"   ├── basic.vim        - Core Vim settings and behavior
"   ├── plugins.vim      - Plugin management with vim-plug
"   ├── keymaps.vim      - Key mappings and shortcuts
"   ├── completion.vim   - CoC.nvim and completion configuration
"   ├── writing.vim      - VimPencil and writing-focused settings
"   └── programming.vim  - Language-specific programming settings
"
" File: vim/.vimrc
" ============================================================================

" Ensure we know where the configuration files are located
let g:vim_config_dir = expand('~/.vim/config')

" Ensure the config directory exists
if !isdirectory(g:vim_config_dir)
    echoerr 'Configuration directory not found: ' . g:vim_config_dir
    echoerr 'Please ensure all configuration modules are properly installed.'
    finish
endif

" ============================================================================
" MODULE LOADING FUNCTIONS
" ============================================================================

" Safe source function with error handling
function! s:SourceModule(module_name) abort
    let l:module_path = g:vim_config_dir . '/' . a:module_name . '.vim'

    if !filereadable(l:module_path)
        echoerr 'Configuration module not found: ' . l:module_path
        return 0
    endif

    try
        execute 'source ' . fnameescape(l:module_path)
        return 1
    catch
        echoerr 'Error loading module ' . a:module_name . ': ' . v:exception
        return 0
    endtry
endfunction

" Function to check if all required modules exist
function! s:CheckModules() abort
    let l:required_modules = ['basic', 'plugins', 'keymaps', 'completion', 'writing', 'programming']
    let l:missing_modules = []

    for l:module in l:required_modules
        let l:module_path = g:vim_config_dir . '/' . l:module . '.vim'
        if !filereadable(l:module_path)
            call add(l:missing_modules, l:module)
        endif
    endfor

    if !empty(l:missing_modules)
        echoerr 'Missing configuration modules: ' . join(l:missing_modules, ', ')
        echoerr 'Please ensure all modules are properly installed in: ' . g:vim_config_dir
        return 0
    endif

    return 1
endfunction

" ============================================================================
" LOAD CONFIGURATION MODULES
" ============================================================================

" Check that all modules exist before proceeding
if !s:CheckModules()
    finish
endif

" Load configuration modules in proper dependency order:

" 1. BASIC SETTINGS (must be first)
"    Core Vim settings, appearance, and basic behavior
if !s:SourceModule('basic')
    echoerr 'Failed to load basic configuration. Cannot continue.'
    finish
endif

" 2. PLUGIN MANAGEMENT (must be early for plugin availability)
"    vim-plug setup and all plugin declarations
if !s:SourceModule('plugins')
    echoerr 'Failed to load plugin configuration.'
endif

" 3. KEY MAPPINGS (after plugins, before specialized features)
"    All key bindings, mappings, and keyboard shortcuts
if !s:SourceModule('keymaps')
    echoerr 'Failed to load keymap configuration.'
endif

" 4. COMPLETION AND LSP (after plugins and keymaps)
"    CoC.nvim and completion-related configuration
if !s:SourceModule('completion')
    echoerr 'Failed to load completion configuration.'
endif

" 5. WRITING CONFIGURATION (after completion for proper CoC disabling)
"    VimPencil and writing-focused settings
if !s:SourceModule('writing')
    echoerr 'Failed to load writing configuration.'
endif

" 6. PROGRAMMING SETTINGS (last, may depend on all previous modules)
"    Language-specific programming settings and tools
if !s:SourceModule('programming')
    echoerr 'Failed to load programming configuration.'
endif

" ============================================================================
" POST-LOAD VALIDATION AND INFORMATION
" ============================================================================

" Function to show configuration status
function! s:ShowConfigStatus() abort
    echo '=== Modular Vim Configuration Loaded ==='
    echo 'Configuration directory: ' . g:vim_config_dir
    echo 'Modules loaded:'

    let l:modules = ['basic', 'plugins', 'keymaps', 'completion', 'writing', 'programming']
    for l:module in l:modules
        let l:status = filereadable(g:vim_config_dir . '/' . l:module . '.vim') ? '✓' : '✗'
        echo '  ' . l:status . ' ' . l:module . '.vim'
    endfor

    if exists('g:plugs')
        echo 'Plugins managed by vim-plug: ' . len(g:plugs) . ' declared'
    endif

    if exists('g:did_coc_loaded')
        echo 'CoC.nvim: loaded'
    endif

    echo 'Configuration loaded successfully!'
endfunction

" Command to show configuration status
command! VimConfigStatus call <SID>ShowConfigStatus()

" ============================================================================
" HELPFUL COMMANDS AND ALIASES
" ============================================================================

" Command to edit configuration modules quickly
command! -nargs=1 -complete=custom,<SID>ConfigComplete VimConfigEdit
    \ execute 'edit ' . g:vim_config_dir . '/' . <q-args> . '.vim'

" Command to reload all configuration modules
command! VimConfigReload source $MYVIMRC

" Tab completion for configuration module names
function! s:ConfigComplete(ArgLead, CmdLine, CursorPos) abort
    let l:modules = ['basic', 'plugins', 'keymaps', 'completion', 'writing', 'programming']
    return join(filter(l:modules, 'v:val =~ "^" . a:ArgLead'), "\n")
endfunction

" ============================================================================
" STARTUP MESSAGE
" ============================================================================

" Show a brief startup message about the modular configuration
autocmd VimEnter * if has('cmdline_info') |
    \ echom 'Modular Vim configuration loaded. Use :VimConfigStatus for details.' |
    \ endif

" ============================================================================
" CONFIGURATION NOTES AND HELP
" ============================================================================

" Quick help for the modular configuration:
"
" Commands:
"   :VimConfigStatus     - Show status of all configuration modules
"   :VimConfigEdit <module> - Edit a specific configuration module
"   :VimConfigReload     - Reload the entire configuration
"
" Module editing examples:
"   :VimConfigEdit basic        - Edit basic settings
"   :VimConfigEdit plugins      - Edit plugin configuration
"   :VimConfigEdit keymaps      - Edit key mappings
"   :VimConfigEdit completion   - Edit CoC and completion settings
"   :VimConfigEdit writing      - Edit writing-focused configuration
"   :VimConfigEdit programming  - Edit programming language settings
"
" File locations:
"   Main config: ~/.vimrc (this file)
"   Modules: ~/.vim/config/*.vim
"   Plugin storage: ~/.vim/plugged/
"   CoC config: ~/.vim/coc-settings.json