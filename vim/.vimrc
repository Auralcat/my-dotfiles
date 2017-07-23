"ToggledSettings:
set nocompatible              " be iMproved, required
filetype off                  " required
filetype plugin on

set wildmenu " Enable wild menu
set title " Shows title of the file
syntax on " Enables syntax by default

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

" Required for VimOrganizer:
filetype plugin indent on

au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org call org#SetOrgFileType()

"VimPencil settings:"
augroup pencil
    autocmd!
    autocmd FileType markdown, mkd call pencil#init()
    autocmd FileType text call pencil#init({'wrap': 'soft'})
augroup END

" Templates:

augroup templates
    " Templates for Project Euler
    autocmd BufNewFile euler*.py 0r ~/.vim/templates/skeleton_euler.py
    autocmd BufNewFile euler*.rb 0r ~/.vim/templates/skeleton_euler.rb
    " General templates
    autocmd BufNewFile *.rb 0r ~/.vim/templates/skeleton.rb
    autocmd BufNewFile *.py 0r ~/.vim/templates/skeleton.py
    " Scratch files
    autocmd BufNewFile scratch.md 0r ~/.vim/templates/skeleton.scratch
    " Setting a few flags to unregister the scratch buffer
    autocmd BufNewFile scratch.md setlocal bufhidden=hide
    autocmd BufNewFile scratch.md setlocal noswapfile
augroup END

" ProgrammingMode:
augroup programming
    autocmd!
    autocmd FileType python,ruby,c,cpp,java set number "Shows line numbers"
    autocmd FileType python,ruby,c,cpp,java set ruler "Shows row and column numbers"
    autocmd FileType python,ruby,c,cpp,java set showcmd "Shows incomplete commands"
    "Activate rainbow parentheses based on file type:
    autocmd FileType python,ruby,c,cpp,java RainbowParentheses
    " Wraps text before column 80.
    autocmd FileType python,ruby,c,cpp,java set tw=79
    autocmd FileType python,ruby,c,cpp,java set cc=80
    " Colorscheme for Python files
    autocmd FileType python :colorscheme desert
    " Run Python and Ruby scripts with F6
    autocmd FileType python nnoremap <F6> :!python3 %<CR>
    autocmd FileType ruby nnoremap <F6> :!ruby %<CR>
    " Compile and run C Programs with F6 with some help from the shell
    " autocmd FileType cpp nnoremap <F6> :!g -o
augroup END

" Text Mode:
augroup writing
    autocmd!
    autocmd FileType text set nonumber
    autocmd FileType text set noruler
    autocmd FileType text SoftPencil
augroup END

" Encryption
set nobackup
set nowritebackup
set noswapfile
set cm=blowfish2

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

" Same bindings for normal mode as well
nnoremap <C-a> ^
nnoremap <C-e> $

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

" TabNavigation:
" Create new tab
noremap <leader>t :tabnew<CR>

" Close tab
noremap <leader>w :tabclose<CR>

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

" PLUGINS:

" AutoPairs: insert or delete brackets, parens, quotes in pair
Plugin 'jiangmiao/auto-pairs'

" VimOrganizer: VimOrganizer is partly a clone of Emacs' Org-mode, and partly
" a front end to Org-mode itself. Do Org in Vim.
Plugin 'hsitz/vimorganizer'

" MultipleCursors: Creates multiple cursors where you can type
" at the same time. Replicates the feature from Sublime Text/Atom.
Plugin 'terryma/vim-multiple-cursors'

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

"________SNIPPETS______________________________________________________________
