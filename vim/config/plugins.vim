" ============================================================================
" PLUGIN MANAGEMENT
" ============================================================================
" All vim-plug plugin declarations and automatic plugin installation
" File: vim/config/plugins.vim

" ============================================================================
" VIM-PLUG AUTO-INSTALLATION
" ============================================================================

" Install vim-plug if not found
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

" ============================================================================
" PLUGIN DECLARATIONS
" ============================================================================

call plug#begin('~/.vim/plugged')

" ----------------------------------------------------------------------------
" CORE PLUGINS (always loaded)
" ----------------------------------------------------------------------------

" AutoPairs: insert or delete brackets, parens, quotes in pair
Plug 'jiangmiao/auto-pairs'

" VimSurround: Better bracket/surround symbol control in Vim
Plug 'tpope/vim-surround'

" EditorConfig: Keep standard configurations between editors and projects
Plug 'editorconfig/editorconfig-vim'

" ----------------------------------------------------------------------------
" DEVELOPMENT TOOLS (lazy loaded for better performance)
" ----------------------------------------------------------------------------

" ALE: Asynchronous Lint Engine - load only for programming files
Plug 'dense-analysis/ale', {
    \ 'for': [
        \ 'python', 'ruby', 'c', 'cpp', 'java', 'javascript',
        \ 'css', 'html', 'go', 'rust', 'php', 'typescript'
    \ ]
\ }

" Coc.nvim: LSP support - load for supported file types
Plug 'neoclide/coc.nvim', {
    \ 'branch': 'release',
    \ 'for': [
        \ 'python', 'javascript', 'typescript', 'c', 'cpp', 'java',
        \ 'ruby', 'css', 'html', 'json', 'yaml', 'xml', 'go',
        \ 'rust', 'php', 'vim'
    \ ]
\ }

" RainbowParentheses: Better parentheses highlighting - load only for programming files
Plug 'junegunn/rainbow_parentheses.vim', {
    \ 'for': [
        \ 'python', 'ruby', 'c', 'cpp', 'java', 'javascript',
        \ 'go', 'rust', 'php', 'typescript'
    \ ]
\ }

" ----------------------------------------------------------------------------
" PHP-SPECIFIC PLUGINS (lazy loaded for PHP development)
" ----------------------------------------------------------------------------

" StanAngeloff/php.vim: Enhanced PHP syntax highlighting and indentation
Plug 'StanAngeloff/php.vim', {'for': 'php'}

" 2072/PHP-Indenting-for-VIm: Better PHP indentation
Plug '2072/PHP-Indenting-for-VIm', {'for': 'php'}

" ----------------------------------------------------------------------------
" WRITING TOOLS (lazy loaded)
" ----------------------------------------------------------------------------

" VimPencil: Soft line wrap and hard line breaks - load only for text files
Plug 'reedes/vim-pencil', {
    \ 'for': ['markdown', 'text', 'org', 'rst', 'asciidoc']
\ }

" ----------------------------------------------------------------------------
" VISUAL ENHANCEMENTS (on demand)
" ----------------------------------------------------------------------------

" VimColorSchemes: plugin to manage colorschemes - load on demand
Plug 'flazz/vim-colorschemes', {'on': 'Colorscheme'}

call plug#end()

" ============================================================================
" PLUGIN HELP AND COMMANDS
" ============================================================================

" Brief help for vim-plug:
" :PlugInstall    - installs plugins
" :PlugUpdate     - updates plugins
" :PlugClean      - removes unlisted plugins
" :PlugUpgrade    - upgrades vim-plug itself
" :PlugStatus     - check plugin status
" :PlugDiff       - examine changes from the previous update

" ============================================================================
" PLUGIN-SPECIFIC EARLY INITIALIZATION
" ============================================================================

" VimOrganizer requirements (legacy support)
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org call org#SetOrgFileType()