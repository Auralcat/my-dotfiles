"ToggledSettings:
set nocompatible              " be iMproved, required
filetype off                  " required
filetype plugin on

" Enable statusline regardless of whether you have more than two windows:
" set laststatus=2

"VimPencil settings:"
augroup pencil
    autocmd!
    autocmd FileType markdown, mkd call pencil#init()
    autocmd FileType text call pencil#init({'wrap': 'soft'})
augroup END

" ProgrammingMode:
"Activate rainbow parentheses based on file type:
augroup programming
    autocmd!
    autocmd FileType python,ruby,c,java RainbowParentheses
    autocmd FileType python,ruby,c,java set cc=80
    " Colorscheme for Python files
    autocmd FileType python :colorscheme desert
augroup END

" Text Mode:
augroup writing
    autocmd!
    autocmd FileType text SoftPencil
augroup END

set title "Shows title of the file (look up! ^)"
set number " Shows line numbers"
set ruler " Shows row and column numbers"
set showcmd "Shows incomplete commands"

" COLORS:
"Color Scheme"
"colorscheme desert
"colorscheme wombat
"colorscheme molokai
"colorscheme vividchalk

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

" Adapting Emacs word movement to Vim:
imap <C-b> <C-o>b
imap <C-f> <C-o>w

" I'm using C-d here because it won't terminate Vim. If I press it at the
" wrong time, like in normal mode, it'll just scroll the page down.
imap <C-d> <C-o>dW

" Quicker window navigation:
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" Quicker window closing. It's OK to use C-c since if you don't bind any
" shortcuts to it, you'll get a message saying 'please quit vim with :quit'.
" It's also a bonus quick button if there's only one window in the session.
nnoremap <C-c> <C-w>q

" For when you're stuck:
nnoremap <F2> :!python3 ~/oblique-strategies/oblique.py<CR>

" FileNavigation:

" NERDTree hotkeys:

noremap <leader>f :NERDTree<CR>
noremap <leader>q :NERDTreeClose<CR>

" TabNavigation:
" Create new tab
noremap <leader>t :tabnew<CR>

" Close tab
noremap <leader>w :tabclose<CR>

" PgUp/PgDown navigation
noremap <leader><PageUp> gt
noremap <leader><PageDown> gT
noremap <leader>k gt
noremap <leader>j gT
" Cycle through tabs by number:
noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt
noremap <leader>0 :tablast<cr>

"______________________________________________________________________________

" Installing plugins:
" - Type :source %
" - Type :PluginInstall

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')
" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
" EXAMPLES:
" Git plugin not hosted on GitHub
" Plugin 'git://git.wincent.com/command-t.git'
" git repos on your local machine (i.e. when working on your own plugin)
" Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
" ENDEXAMPLES:

Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Install L9 and avoid a Naming conflict if you've already installed a
" different version somewhere else.
" PLUGINS:

" YouCompleteMe: Autocomplete plugin
Plugin 'valloric/youcompleteme'

" vim-snippets: exactly what says on the tin
Plugin 'honza/vim-snippets'

" Track the engine.
Plugin 'SirVer/ultisnips'
"
" Trigger configuration. Do not use <tab> if you use YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-c>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
"
" " If you want :UltiSnipsEdit to split your window.
" let g:UltiSnipsEditSplit="vertical"

" AutoPairs: insert or delete brackets, parens, quotes in pair
Plugin 'jiangmiao/auto-pairs'

" NERDTree: a tree explorer for Vim
Plugin 'scrooloose/nerdtree'

" MultipleCursors: Creates multiple cursors where you can type
" at the same time. Replicates the feature from Sublime Text/Atom.
Plugin 'terryma/vim-multiple-cursors'

" VimRuby: Ruby-related tools.
Plugin 'vim-ruby/vim-ruby'

"automatically remove all trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

" Syntastic: Checks syntax of source code.
Plugin 'scrooloose/syntastic'

" VimPencil: Soft line wrap and hard line breaks
Plugin 'reedes/vim-pencil'

" RainbowParentheses: Better parentheses
Plugin 'junegunn/rainbow_parentheses.vim'

" VimColorSchemes: plugin to manage colorschemes
Plugin 'flazz/vim-colorschemes'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
"
" Set tab length to 4
:set tabstop=4
" Change number of space chars inserted for indentation
:set shiftwidth=4
:set expandtab

" MAPPINGS:
:map <F5> :update<CR>

" ENDMAPPINGS:
