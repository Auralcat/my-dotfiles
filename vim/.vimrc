"ToggledSettings:
set nocompatible              " be iMproved, required
filetype off                  " required
filetype plugin on

"Default colorscheme
:colorscheme elflord

set laststatus=0 " I don't like the statusline
set wildmenu " Enable wild menu
set title " Shows title of the file
syntax on " Enables syntax by default
set incsearch " Shows search results as you type them

" JuryRiggings:
set noswapfile " Swapfiles are annoying!
set backspace=2 " Returns common backspace function to insert mode

" Set graphical font, mostly for Vim Anywhere, I like that idea!
set guifont=Fantasque\ Sans\ Mono\ Regular

" Default tabstop
set tabstop=4
" change number of space chars inserted for indentation
set shiftwidth=4
set expandtab

" Fixes newline insertion problem in insert mode
inoremap <CR> <CR>

" Enable fuzzy finding files through the :find command
" and provides tab completion for all file-related tasks
set path+=**

" Make tags for easier navigation inside the file with C-], gC-](that's g plus
" C-]) and C-t (helps with completion as well)
command! MakeTags !ctags -R .

" Enable dictionary completion by pointing to the dict file
set dictionary+=/usr/share/dict/american-english
set dictionary+=/usr/share/dict/brazilian-portuguese

" Enable omnicompletion:
set omnifunc=syntaxcomplete#Complete

" Set completion behavior to highlight longest word by default and
" show the menu even if there's only one match
set completeopt=menuone,preview,longest

" Required for VimOrganizer:
filetype plugin indent on

au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org call org#SetOrgFileType()

"VimPencil settings:"
augroup pencil
    autocmd!
    autocmd FileType markdown, call pencil#init()
    autocmd FileType text call pencil#init({'wrap': 'soft'})
augroup END

" Templates:

augroup templates
    " Templates for Project Euler
    autocmd BufNewFile euler*.py 0r ~/.vim/templates/skeleton_euler.py
    autocmd BufNewFile euler*.rb 0r ~/.vim/templates/skeleton_euler.rb
    autocmd BufNewFile Euler*.java 0r ~/.vim/templates/skeleton_euler.java
    autocmd BufNewFile euler*.cpp 0r ~/.vim/templates/skeleton_euler.cpp
    " General templates
    autocmd BufNewFile *.rb 0r ~/.vim/templates/skeleton.rb
    autocmd BufNewFile *.py 0r ~/.vim/templates/skeleton.py
augroup END

" ScratchMode: (this was copied from Emacs)
augroup scratchfile
    " Header for the scratch file
    autocmd BufNewFile scratch.md 0r ~/.vim/templates/skeleton.scratch
    " Setting a few flags to unregister the scratch buffer
    autocmd BufNewFile scratch.md setlocal bufhidden=hide
    autocmd BufNewFile scratch.md setlocal noswapfile
    autocmd BufNewFile scratch.md setlocal viminfo=
augroup END

" PASTE MODE CONFIGURATION
" =========================
" Enable bracketed paste mode for modern terminals
" This automatically detects when text is being pasted and disables autoindent
if has('patch-8.0.0238') || has('nvim-0.2.3')
    " Modern Vim/Neovim has built-in bracketed paste support
    " which automatically handles paste mode
    set t_BE=
endif

" Manual paste mode toggle with F2
set pastetoggle=<F2>

" Smart paste function that temporarily disables problematic settings
function! SmartPaste()
    set paste
    normal! "+p
    set nopaste
endfunction

" Key mappings for smart pasting
nnoremap <leader>p :call SmartPaste()<CR>
nnoremap <leader>P :call SmartPaste()<CR>

" Insert mode paste that preserves indentation
inoremap <C-v> <C-o>:set paste<CR><C-r>+<C-o>:set nopaste<CR>

" PYTHON-SPECIFIC INDENTATION SETTINGS
" ====================================

" ProgrammingMode:
augroup programming
    autocmd!
    autocmd FileType python,ruby,c,cpp,java,javascript set number "Shows line numbers"
    autocmd FileType python,ruby,c,cpp,java,javascript set ruler "Shows row and column numbers"
    autocmd FileType python,ruby,c,cpp,java,javascript set showcmd "Shows incomplete commands"
    "Activate rainbow parentheses based on file type:
    autocmd FileType python,ruby,c,cpp,java,javascript RainbowParentheses
    " Ruby: set tab length to 2
    " OVERRIDEN BY EDITORCONFIG!!
    autocmd FileType ruby set tabstop=2
    autocmd FileType ruby set shiftwidth=2
    " Wraps text before column 80.
    autocmd FileType python,ruby,c,cpp,java,javascript set tw=79
    autocmd FileType python,ruby,c,cpp,java,javascript set cc=80
    " Colorschemes for Python and Ruby files
    autocmd FileType python :colorscheme desert
    autocmd FileType ruby :colorscheme landscape
    " Run Python, JS and Ruby scripts with F6
    autocmd FileType python nnoremap <F6> :!python3 %<CR>
    autocmd FileType ruby nnoremap <F6> :!ruby %<CR>
    autocmd FileType javascript nnoremap <F6> :!nodejs % <CR>
    " Compile and run C Programs with F6 with some help from the shell
    " autocmd FileType cpp nnoremap <F6> :!g -o
    " Enable omnicompletion
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete

    " PYTHON-SPECIFIC PASTE AND INDENTATION SETTINGS
    " ===============================================
    " Ensure proper Python indentation settings
    autocmd FileType python setlocal autoindent
    autocmd FileType python setlocal smartindent
    autocmd FileType python setlocal cindent
    autocmd FileType python setlocal indentkeys-=<:>
    autocmd FileType python setlocal indentkeys-=:
    " Python-specific paste mappings that preserve indentation
    autocmd FileType python nnoremap <buffer> <leader>pp :set paste<CR>o<C-r>+<Esc>:set nopaste<CR>
    autocmd FileType python nnoremap <buffer> <leader>pP :set paste<CR>O<C-r>+<Esc>:set nopaste<CR>
    " Better Python indentation handling for pasted code
    autocmd FileType python inoremap <buffer> <C-v> <C-o>:set paste<CR><C-r>+<C-o>:set nopaste<CR><C-o>:silent! normal ==<CR>
    " Command to fix indentation of pasted Python code
    autocmd FileType python command! -buffer FixPythonIndent normal! gg=G
    autocmd FileType python nnoremap <buffer> <leader>= :FixPythonIndent<CR>
augroup END

" ADDITIONAL PASTE UTILITIES
" ==========================
" Function to clean up common paste artifacts in Python
function! CleanPythonPaste()
    " Remove common artifacts from web/email pasting
    silent! %s/^\s*>>> //g
    silent! %s/^\s*\.\.\. //g
    " Remove trailing whitespace
    silent! %s/\s\+$//g
    " Fix common indentation issues
    normal! gg=G
endfunction

" Command to clean pasted Python code
command! CleanPythonPaste call CleanPythonPaste()

" Text Mode:
augroup writing
    autocmd!
    autocmd FileType text set nonumber
    autocmd FileType text set noruler
    autocmd FileType text SoftPencil
augroup END

" TodoCommands:
augroup todo
    " Put a DONE stamp on the selected item.
    autocmd BufReadPre .todo.md nnoremap <C-i> ^wi*[DONE]*<space><C-[>]
augroup END
"Quick update files:
nnoremap <F5> :update<CR>

"automatically remove all trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

" HardMode:

map <up> :echoe "Please use k."<CR>
map <down> :echoe "Please use j."<CR>
map <left> :echoe "Please use h."<CR>
map <right> :echoe "Please use l."<CR>

" There's no echo in insert mode because you'd insert the :echoe line in it
" instead.
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" Set Emacs bindings for insert mode. This is handy when you're typing
" autocompleted brackets/parentheses/quotes.
imap <C-a> <C-o>0
imap <C-e> <C-o>$

" VimEmmet: Keep keybinding compatibility with Emacs
let g:user_emmet_leader_key='<C-j>'

" Quicker window navigation:
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" Quicker window closing. It's OK to use C-c since if you don't bind any
" shortcuts to it, you'll get a message saying 'please quit vim with :quit'.
" It's also a bonus quick button if there's only one window in the session.
nnoremap <C-c> <C-w>q

" TabNavigation:
" Create new tab
noremap <leader>t :tabnew<CR>

" Close tab
noremap <leader>w :tabclose<CR>

"______________________________________________________________________________

" Installing plugins with vim-plug:
" - Type :source %
" - Type :PlugInstall

" Install vim-plug if not found
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

" PLUGINS:
call plug#begin('~/.vim/plugged')

" AutoPairs: insert or delete brackets, parens, quotes in pair
Plug 'jiangmiao/auto-pairs'

" ALE: Asynchronous Lint Engine (modern replacement for Syntastic)
Plug 'dense-analysis/ale'

" VimPencil: Soft line wrap and hard line breaks
Plug 'reedes/vim-pencil'

" RainbowParentheses: Better parentheses
Plug 'junegunn/rainbow_parentheses.vim'

" VimColorSchemes: plugin to manage colorschemes
Plug 'flazz/vim-colorschemes'

" VimSurround: Better bracket/surround symbol control in Vim
Plug 'tpope/vim-surround'

" EditorConfig: Keep standard configurations between editors and projects
Plug 'editorconfig/editorconfig-vim'

" Coc.nvim: LSP support with autocompletion, diagnostics, and more
Plug 'neoclide/coc.nvim', {'branch': 'release'}

call plug#end()

" Brief help for vim-plug
" :PlugInstall    - installs plugins
" :PlugUpdate     - updates plugins
" :PlugClean      - removes unlisted plugins
" :PlugUpgrade    - upgrades vim-plug itself
"

"________SNIPPETS______________________________________________________________
