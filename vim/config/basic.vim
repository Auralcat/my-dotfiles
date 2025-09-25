" ============================================================================
" BASIC VIM CONFIGURATION
" ============================================================================
" Core Vim settings, appearance, and basic behavior
" File: vim/config/basic.vim

" Ensure vi compatibility is disabled
set nocompatible

" Enable filetype detection and plugins
filetype off
filetype plugin on
filetype plugin indent on

" ============================================================================
" DISPLAY AND APPEARANCE
" ============================================================================

" Default colorscheme
colorscheme elflord

" Enable syntax highlighting
syntax on

" Show statusline when multiple windows exist (needed for CoC)
set laststatus=1

" Enable wild menu for command completion
set wildmenu

" Show title of the file in window title
set title

" Always show signcolumns (needed for LSP diagnostics)
set signcolumn=yes

" Better display for messages
set cmdheight=2

" ============================================================================
" SEARCH AND NAVIGATION
" ============================================================================

" Show search results as you type
set incsearch

" Enable fuzzy finding files through the :find command
" and provides tab completion for all file-related tasks
set path+=**

" ============================================================================
" BASIC BEHAVIOR
" ============================================================================

" Disable swap files (they can be annoying)
set noswapfile

" Return common backspace function to insert mode
set backspace=2

" Set internal encoding of vim
set encoding=utf-8

" TextEdit might fail if hidden is not set
set hidden

" You will have bad experience for diagnostic messages when it's default 4000
set updatetime=300

" Don't pass messages to ins-completion-menu
set shortmess+=c

" ============================================================================
" INDENTATION AND TABS
" ============================================================================

" Default tab settings
set tabstop=4
set shiftwidth=4
set expandtab

" ============================================================================
" FONT AND GUI SETTINGS
" ============================================================================

" Set graphical font, mostly for Vim Anywhere
set guifont=Fantasque\ Sans\ Mono\ Regular

" ============================================================================
" FILE MANAGEMENT
" ============================================================================

" Enable dictionary completion by pointing to the dict files
set dictionary+=/usr/share/dict/american-english
set dictionary+=/usr/share/dict/brazilian-portuguese

" ============================================================================
" COMPLETION SETTINGS
" ============================================================================

" Enable omnicompletion
set omnifunc=syntaxcomplete#Complete

" Set completion behavior to highlight longest word by default and
" show the menu even if there's only one match
set completeopt=menuone,preview,longest

" ============================================================================
" TAGS AND NAVIGATION
" ============================================================================

" Make tags for easier navigation inside the file with C-], gC-] and C-t
" (helps with completion as well)
command! MakeTags !ctags -R .

" ============================================================================
" AUTOMATIC CLEANUP
" ============================================================================

" Automatically remove all trailing spaces on save
autocmd BufWritePre * :%s/\s\+$//e

" ============================================================================
" FINAL WARNINGS AND CHECKS
" ============================================================================

" Final verification that updatetime is appropriate for LSP
if &updatetime > 1000
    echom 'Warning: updatetime is ' . &updatetime . 'ms, which may cause slow LSP responses. Consider setting it to 300ms.'
endif