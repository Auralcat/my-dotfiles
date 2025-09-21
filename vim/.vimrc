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

" OPTIMIZED PASTE MODE CONFIGURATION
" ==================================
" Lazy-loaded paste functions to minimize startup overhead
let g:paste_optimized = 1

" Enhanced bracketed paste for modern terminals
if has('patch-8.0.0238') || has('nvim-0.2.3')
    " Enable bracketed paste with proper escape sequences
    let &t_SI .= "\<Esc>[?2004h"
    let &t_EI .= "\<Esc>[?2004l"
    let &t_BE = "\<Esc>[?2004h"
endif

" Fast paste mode toggle
set pastetoggle=<F2>

" Optimized smart paste with single mode switch
function! s:SmartPaste() abort
    let l:paste_state = &paste
    set paste
    execute 'normal! "+p'
    let &paste = l:paste_state
endfunction

" Efficient paste mappings
nnoremap <silent> <leader>p :call <SID>SmartPaste()<CR>
nnoremap <silent> <leader>P :call <SID>SmartPaste()<CR>

" OPTIMIZED PROGRAMMING CONFIGURATION
" ===================================

" Core programming settings (consolidated for performance)
augroup programming
    autocmd!
    autocmd FileType python,ruby,c,cpp,java,javascript setlocal number ruler showcmd tw=79 cc=80
    autocmd FileType python,ruby,c,cpp,java,javascript if exists(':RainbowParentheses') | RainbowParentheses | endif
    " Ruby-specific settings
    autocmd FileType ruby setlocal tabstop=2 shiftwidth=2
    " Python indentation now handled by EditorConfig - removed redundant settings
    " Colorschemes
    autocmd FileType python colorscheme desert
    autocmd FileType ruby colorscheme landscape
    " Run scripts
    autocmd FileType python nnoremap <buffer> <F6> :!python3 %<CR>
    autocmd FileType ruby nnoremap <buffer> <F6> :!ruby %<CR>
    autocmd FileType javascript nnoremap <buffer> <F6> :!nodejs %<CR>
    " Omnicompletion
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
    " Python-specific optimizations (consolidated from python_optimized group)
    autocmd FileType python call <SID>SetupPythonPaste()
    autocmd FileType python setlocal cindent
    " Python auto-formatting with black
    autocmd BufWritePre *.py call <SID>FormatWithBlack()
augroup END

" OPTIMIZED PYTHON-SPECIFIC CONFIGURATION
" =======================================
" Python auto-formatting with black configuration
" Set g:python_black_enabled = 0 to disable auto-formatting globally
" Use :BlackToggle to toggle auto-formatting for current session
" Use :BlackFormat to manually format current file
" Key mappings (in Python files): <leader>bf (format), <leader>bt (toggle)
let g:python_black_enabled = 1
let g:python_black_check_performed = 0
let g:python_black_available = 0

" Lazy-loaded Python indentation functions
let g:python_paste_optimized = 0

function! s:SetupPythonPaste() abort
    if g:python_paste_optimized | return | endif
    let g:python_paste_optimized = 1

    " Python indentation now handled by EditorConfig - only set smart indentation
    setlocal autoindent smartindent
    setlocal indentkeys-=<:> indentkeys-=:

    " Fast Python paste with limited reindentation (max 50 lines)
    nnoremap <buffer><silent> <leader>pp :call <SID>PythonPasteBelow()<CR>
    nnoremap <buffer><silent> <leader>pP :call <SID>PythonPasteAbove()<CR>
    nnoremap <buffer><silent> <leader>= :call <SID>FixPythonIndentLimited()<CR>

    " Black formatting key mappings
    nnoremap <buffer><silent> <leader>bf :BlackFormat<CR>
    nnoremap <buffer><silent> <leader>bt :BlackToggle<CR>
endfunction

" Efficient Python paste functions with limited scope
function! s:PythonPasteBelow() abort
    let l:start_line = line('.')
    set paste
    normal! o
    silent put +
    set nopaste
    call s:IndentPastedLines(l:start_line + 1, line('.'))
endfunction

function! s:PythonPasteAbove() abort
    let l:start_line = line('.')
    set paste
    normal! O
    silent put! +
    set nopaste
    call s:IndentPastedLines(line('.'), l:start_line)
endfunction

" Smart indentation limited to paste area (max 50 lines)
function! s:IndentPastedLines(start, end) abort
    let l:line_count = a:end - a:start + 1
    if l:line_count > 50
        echo 'Large paste detected (' . l:line_count . ' lines). Skipping auto-indent for performance.'
        return
    endif
    execute a:start . ',' . a:end . 'normal! =='
endfunction

" Limited scope indentation fix for performance
function! s:FixPythonIndentLimited() abort
    let l:total_lines = line('$')
    if l:total_lines > 1000
        let l:choice = confirm('Large file (' . l:total_lines . ' lines). Indent entire file?', "&Yes\n&No\n&Current function only", 2)
        if l:choice == 1
            normal! gg=G
        elseif l:choice == 3
            normal! [z=][
        endif
    else
        normal! gg=G
    endif
endfunction

" PYTHON BLACK AUTO-FORMATTING
" =============================
" Check black availability once per session
function! s:CheckBlackAvailability() abort
    if g:python_black_check_performed | return g:python_black_available | endif
    let g:python_black_check_performed = 1

    if !executable('black')
        let g:python_black_available = 0
        echo 'Black not found in PATH. Python auto-formatting disabled.'
        return 0
    endif

    let g:python_black_available = 1
    return 1
endfunction

" Format current buffer with black
function! s:FormatWithBlack() abort
    " Check if auto-formatting is enabled
    if !g:python_black_enabled | return | endif

    " Check black availability (cached after first check)
    if !s:CheckBlackAvailability() | return | endif

    " Save cursor position and view
    let l:view = winsaveview()
    let l:cursor_line = line('.')
    let l:cursor_col = col('.')

    " Get current buffer content
    let l:original_content = join(getline(1, '$'), "\n")

    " Create temporary file for black processing
    let l:temp_file = tempname() . '.py'
    call writefile(split(l:original_content, "\n"), l:temp_file)

    " Run black on temporary file
    let l:black_cmd = 'black --quiet --fast ' . shellescape(l:temp_file) . ' 2>&1'
    let l:result = system(l:black_cmd)
    let l:exit_code = v:shell_error

    if l:exit_code == 0
        " Black succeeded, read formatted content
        let l:formatted_content = readfile(l:temp_file)
        let l:new_content = join(l:formatted_content, "\n")

        " Only update buffer if content changed
        if l:new_content !=# l:original_content
            " Replace buffer content
            silent! %delete _
            call setline(1, l:formatted_content)

            " Restore cursor position (approximate)
            call winrestview(l:view)
            " Try to maintain cursor position, fallback to original line
            if line('$') >= l:cursor_line
                call cursor(l:cursor_line, l:cursor_col)
            else
                call cursor(line('$'), 1)
            endif

            echo 'Python code formatted with black'
        endif
    else
        " Black failed, show error message
        let l:error_msg = substitute(l:result, '\n.*', '', '')
        echo 'Black formatting failed: ' . l:error_msg
    endif

    " Clean up temporary file
    call delete(l:temp_file)
endfunction

" Toggle black auto-formatting
function! s:ToggleBlackFormatting() abort
    let g:python_black_enabled = !g:python_black_enabled
    let l:status = g:python_black_enabled ? 'enabled' : 'disabled'
    echo 'Python black auto-formatting ' . l:status
endfunction

" Manual black formatting command
function! s:FormatCurrentFileWithBlack() abort
    if !s:CheckBlackAvailability()
        echo 'Black is not available. Please install black: pip install black'
        return
    endif
    call s:FormatWithBlack()
endfunction

" Commands for manual control
command! BlackFormat call <SID>FormatCurrentFileWithBlack()
command! BlackToggle call <SID>ToggleBlackFormatting()

" Python-specific autocmds consolidated into programming group above for better performance

" OPTIMIZED PASTE UTILITIES
" ========================
" Efficient Python paste cleanup with limited scope
function! s:CleanPythonPaste() abort
    let l:view = winsaveview()
    " Remove common artifacts from web/email pasting
    silent! %substitute/^\s*>>> //ge
    silent! %substitute/^\s*\.\.\. //ge
    " Remove trailing whitespace
    silent! %substitute/\s\+$//ge
    call winrestview(l:view)
    echo 'Python paste cleaned (artifacts removed)'
endfunction

" Fast cleanup command
command! CleanPythonPaste call <SID>CleanPythonPaste()

" Optimized insert mode paste (single mode switch)
function! s:OptimizedInsertPaste() abort
    return "\<C-o>:set paste\<CR>\<C-r>+\<C-o>:set nopaste\<CR>"
endfunction

inoremap <expr> <C-v> <SID>OptimizedInsertPaste()

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

" Core plugins (always loaded)
" AutoPairs: insert or delete brackets, parens, quotes in pair
Plug 'jiangmiao/auto-pairs'

" VimSurround: Better bracket/surround symbol control in Vim
Plug 'tpope/vim-surround'

" EditorConfig: Keep standard configurations between editors and projects
Plug 'editorconfig/editorconfig-vim'

" Development tools (lazy loaded for better performance)
" ALE: Asynchronous Lint Engine - load only for programming files
Plug 'dense-analysis/ale', {'for': ['python', 'ruby', 'c', 'cpp', 'java', 'javascript', 'css', 'html']}

" Coc.nvim: LSP support - load only for programming files
Plug 'neoclide/coc.nvim', {'branch': 'release', 'for': ['python', 'ruby', 'c', 'cpp', 'java', 'javascript', 'css', 'html', 'typescript']}

" RainbowParentheses: Better parentheses - load only for programming files
Plug 'junegunn/rainbow_parentheses.vim', {'for': ['python', 'ruby', 'c', 'cpp', 'java', 'javascript']}

" Writing tools (lazy loaded)
" VimPencil: Soft line wrap and hard line breaks - load only for text files
Plug 'reedes/vim-pencil', {'for': ['markdown', 'text', 'org']}

" Visual enhancements (on demand)
" VimColorSchemes: plugin to manage colorschemes - load on demand
Plug 'flazz/vim-colorschemes', {'on': 'Colorscheme'}

call plug#end()

" Brief help for vim-plug
" :PlugInstall    - installs plugins
" :PlugUpdate     - updates plugins
" :PlugClean      - removes unlisted plugins
" :PlugUpgrade    - upgrades vim-plug itself
"