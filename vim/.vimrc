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

" Coc.nvim: LSP support
Plug 'neoclide/coc.nvim', {'branch': 'release'}

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

" COC.NVIM CONFIGURATION
" ======================
" Set internal encoding of vim
set encoding=utf-8

" TextEdit might fail if hidden is not set
set hidden

" Better display for messages
set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000
set updatetime=300

" Don't pass messages to |ins-completion-menu|
set shortmess+=c

" Always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <C-space> to trigger completion
if has('nvim')
  inoremap <silent><expr> <C-space> coc#refresh()
else
  inoremap <silent><expr> <C-@> coc#refresh()
endif

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nnoremap <silent> [g <Plug>(coc-diagnostic-prev)
nnoremap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation
nnoremap <silent> gd <Plug>(coc-definition)
nnoremap <silent> gy <Plug>(coc-type-definition)
nnoremap <silent> gi <Plug>(coc-implementation)
nnoremap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call <SID>SafeCocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" SAFE COC CURSORHOLD CONFIGURATION
" ===================================
" Proper autocommand group with error handling for CursorHold events
augroup coc_cursorhold
    autocmd!
    " Only enable for supported file types and when Coc is ready
    autocmd FileType python,javascript,typescript,c,cpp,java,ruby,css,html,json
          \ autocmd CursorHold <buffer> call <SID>SafeCocHighlight()
    " Clear highlights when leaving buffer to prevent conflicts
    autocmd BufLeave * call <SID>ClearCocHighlight()
augroup END

" Safe Coc highlight function with comprehensive error handling
function! s:SafeCocHighlight() abort
    " Check if Coc is available and ready
    if !exists('g:did_coc_loaded') || !get(g:, 'coc_enabled', 1)
        return
    endif

    " Check if current buffer supports Coc features
    if !get(b:, 'coc_enabled', 1) || get(b:, 'coc_suggest_disable', 0)
        return
    endif

    " Verify Coc RPC is ready before making async calls
    if !coc#rpc#ready()
        return
    endif

    " Check if current file type is supported
    let l:supported_types = ['python', 'javascript', 'typescript', 'c', 'cpp', 'java', 'ruby', 'css', 'html', 'json']
    if index(l:supported_types, &filetype) == -1
        return
    endif

    " Only proceed if we're in a normal buffer (not special buffers)
    if &buftype != '' || !&modifiable
        return
    endif

    " Safely call CocActionAsync with error handling
    try
        call CocActionAsync('highlight')
    catch /^Vim\%((\a\+)\)\=:E\d\+/
        " Silently handle any Vim errors to prevent annoying error messages
    catch
        " Log unexpected errors for debugging (optional)
        " echom 'Coc highlight error: ' . v:exception
    endtry
endfunction

" Function to clear Coc highlights safely
function! s:ClearCocHighlight() abort
    if exists('g:did_coc_loaded') && coc#rpc#ready()
        try
            call CocActionAsync('clearHighlight')
        catch
            " Silently handle errors
        endtry
    endif
endfunction

" Symbol renaming
nnoremap <leader>rn <Plug>(coc-rename)

" Formatting selected code
xnoremap <leader>f  <Plug>(coc-format-selected)
nnoremap <leader>f  <Plug>(coc-format-selected)

" Applying codeAction to the selected region
xnoremap <leader>a  <Plug>(coc-codeaction-selected)
nnoremap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer
nnoremap <leader>ac  <Plug>(coc-codeaction)

" Apply AutoFix to problem on the current line
nnoremap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
xnoremap if <Plug>(coc-funcobj-i)
onoremap if <Plug>(coc-funcobj-i)
xnoremap af <Plug>(coc-funcobj-a)
onoremap af <Plug>(coc-funcobj-a)
xnoremap ic <Plug>(coc-classobj-i)
onoremap ic <Plug>(coc-classobj-i)
xnoremap ac <Plug>(coc-classobj-a)
onoremap ac <Plug>(coc-classobj-a)

" Remap <C-f> and <C-b> for scroll float windows/popups
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<C-r>=coc#float#scroll(1)\<CR>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<C-r>=coc#float#scroll(0)\<CR>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use CTRL-S for selections ranges
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer
command! -nargs=0 Format :call <SID>SafeCocActionAsync('format')

" Add `:Fold` command to fold current buffer
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer
command! -nargs=0 OR :call <SID>SafeCocActionAsync('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<CR>
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<CR>
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<CR>
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<CR>
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<CR>
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

" COC PYTHON-SPECIFIC CONFIGURATION
" ==================================
" Enhanced Python LSP integration with existing black formatting
augroup coc_python
    autocmd!
    " Enable Coc for Python files
    autocmd FileType python let b:coc_suggest_disable = 0
    autocmd FileType python let b:coc_diagnostic_disable = 0

    " Enhanced Python-specific Coc keybindings for better workflow
    autocmd FileType python nnoremap <buffer><silent> <leader>gd :call <SID>SafeCocActionAsync('jumpDefinition')<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>gr :call <SID>SafeCocActionAsync('jumpReferences')<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>gi :call <SID>SafeCocActionAsync('jumpImplementation')<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>gt :call <SID>SafeCocActionAsync('jumpTypeDefinition')<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>gD :call <SID>SafeCocActionAsync('jumpDeclaration')<CR>

    " Python hover and documentation
    autocmd FileType python nnoremap <buffer><silent> <leader>h :call <SID>SafeCocActionAsync('doHover')<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>d :call <SID>show_documentation()<CR>
    autocmd FileType python nnoremap <buffer><silent> K :call <SID>show_documentation()<CR>

    " Python code actions and refactoring
    autocmd FileType python nnoremap <buffer><silent> <leader>ca :call <SID>SafeCocActionAsync('codeAction')<CR>
    autocmd FileType python vnoremap <buffer><silent> <leader>ca :call <SID>SafeCocActionAsync('codeAction', 'line')<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>rn :call <SID>SafeCocActionAsync('rename')<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>rf :call <SID>SafeCocActionAsync('refactor')<CR>

    " Python symbol navigation and search
    autocmd FileType python nnoremap <buffer><silent> <leader>s :CocList outline<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>ws :CocList -I symbols<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>fs :CocList -I --ignore-case symbols<CR>

    " Python diagnostics with enhanced navigation
    autocmd FileType python nnoremap <buffer><silent> <leader>dl :CocList diagnostics<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>dn :call <SID>SafeCocActionAsync('diagnosticNext')<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>dp :call <SID>SafeCocActionAsync('diagnosticPrevious')<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>df :call <SID>SafeCocActionAsync('diagnosticInfo')<CR>

    " Enhanced formatting and import management
    autocmd FileType python nnoremap <buffer><silent> <leader>oi :call <SID>SafeCocActionAsync('runCommand', 'editor.action.organizeImport')<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>fm :call <SID>SafeCocActionAsync('format')<CR>
    autocmd FileType python vnoremap <buffer><silent> <leader>fm :call <SID>SafeCocActionAsync('formatSelected')<CR>

    " Python-specific completion and signature help
    autocmd FileType python inoremap <buffer><silent><expr> <C-Space> coc#refresh()
    autocmd FileType python nnoremap <buffer><silent> <leader>sh :call <SID>SafeCocActionAsync('showSignatureHelp')<CR>

    " Enhanced Python workflow - combine organize imports and black formatting
    autocmd FileType python nnoremap <buffer><silent> <leader>af :call <SID>PythonAutoFormat()<CR>

    " Quick LSP status and restart
    autocmd FileType python nnoremap <buffer><silent> <leader>ls :CocCommand workspace.showOutput<CR>
    autocmd FileType python nnoremap <buffer><silent> <leader>lr :CocRestart<CR>
augroup END

" Enhanced Python auto-formatting function that combines LSP and black
function! s:PythonAutoFormat() abort
    " First organize imports via LSP
    call <SID>SafeCocActionAsync('runCommand', 'editor.action.organizeImport')
    " Small delay to ensure imports are organized before black formatting
    call timer_start(100, {-> s:FormatWithBlack()})
    echo 'Auto-formatting: imports organized, applying black...'
endfunction

" COC EXTENSIONS AUTO-INSTALL
" ============================
" Auto-install essential Python and general development extensions
let g:coc_global_extensions = [
  \ 'coc-json',
  \ 'coc-snippets',
  \ 'coc-pairs',
  \ 'coc-word',
  \ 'coc-pyright',
  \ 'coc-clangd',
  \ 'coc-css',
  \ 'coc-html',
  \ 'coc-tsserver',
  \ 'coc-eslint',
  \ 'coc-prettier'
  \ ]

" Coc configuration directory
let g:coc_config_home = expand('~/.vim')

" Note: coc-settings.json is symlinked from dotfiles directory
" Run: ln -sf ~/my-dotfiles/vim/coc-settings.json ~/.vim/coc-settings.json

"
" ENHANCED COC INITIALIZATION AND ERROR HANDLING
" ===============================================
" Improved Coc initialization with proper error handling and status checking
augroup coc_initialization
    autocmd!
    " Enhanced Coc initialization for supported file types
    autocmd FileType python,javascript,typescript,c,cpp,java,ruby,css,html,json call <SID>InitializeCocForBuffer()
    " Handle Coc service ready events
    autocmd User CocServiceReady call <SID>OnCocServiceReady()
    " Handle Coc diagnostics update
    autocmd User CocDiagnosticChange call <SID>OnCocDiagnosticChange()
augroup END

" Initialize Coc for current buffer with safety checks
function! s:InitializeCocForBuffer() abort
    " Check if Coc is available
    if !exists('g:did_coc_loaded')
        return
    endif

    " Enable Coc for this buffer
    let b:coc_enabled = 1
    let b:coc_suggest_disable = 0
    let b:coc_diagnostic_disable = 0

    " Set buffer-specific Coc options
    if &filetype == 'python'
        " Python-specific Coc settings
        let b:coc_root_patterns = ['.git', 'pyproject.toml', 'setup.py', 'requirements.txt']
    endif
endfunction

" Handle Coc service ready event
function! s:OnCocServiceReady() abort
    echo 'Coc.nvim service ready'
endfunction

" Handle diagnostic changes with throttling to prevent spam
let s:diagnostic_timer = -1
function! s:OnCocDiagnosticChange() abort
    " Cancel previous timer if it exists
    if s:diagnostic_timer != -1
        call timer_stop(s:diagnostic_timer)
    endif
    
    " Set a new timer to update diagnostics after a short delay
    let s:diagnostic_timer = timer_start(100, function('s:UpdateDiagnosticDisplay'))
endfunction

function! s:UpdateDiagnosticDisplay(timer) abort
    let s:diagnostic_timer = -1
    " Update statusline or any diagnostic displays here if needed
    " This prevents excessive updates during rapid diagnostic changes
endfunction

" ENHANCED COC ERROR HANDLING
" ===========================
" Wrapper functions for Coc actions with comprehensive error handling

" Safe wrapper for CocActionAsync calls
function! s:SafeCocActionAsync(action, ...) abort
    if !exists('g:did_coc_loaded') || !coc#rpc#ready()
        echom 'Coc.nvim not ready for action: ' . a:action
        return 0
    endif

    try
        if a:0 > 0
            call CocActionAsync(a:action, a:1)
        else
            call CocActionAsync(a:action)
        endif
        return 1
    catch /^Vim\%((.\+)\)\=:E\d\+/
        echom 'Coc action failed: ' . a:action . ' - ' . v:exception
        return 0
    catch
        echom 'Unexpected error in Coc action: ' . a:action . ' - ' . v:exception
        return 0
    endtry
endfunction

" Safe wrapper for CocAction calls
function! s:SafeCocAction(action, ...) abort
    if !exists('g:did_coc_loaded') || !coc#rpc#ready()
        echom 'Coc.nvim not ready for action: ' . a:action
        return v:null
    endif

    try
        if a:0 > 0
            return CocAction(a:action, a:1)
        else
            return CocAction(a:action)
        endif
    catch /^Vim\%((.\+)\)\=:E\d\+/
        echom 'Coc action failed: ' . a:action . ' - ' . v:exception
        return v:null
    catch
        echom 'Unexpected error in Coc action: ' . a:action . ' - ' . v:exception
        return v:null
    endtry
endfunction

" ADDITIONAL COC SAFETY MEASURES
" ==============================
" Ensure Coc extensions are properly managed and errors are handled gracefully

" Enhanced extension management with error handling
function! s:InstallCocExtensions() abort
    if !exists('g:did_coc_loaded') || !coc#rpc#ready()
        echom 'Coc.nvim not ready for extension installation'
        return
    endif

    " Check if extensions are already installed to avoid redundant installation
    let l:installed = CocAction('extensionStats')
    let l:needed_extensions = ['coc-json', 'coc-snippets', 'coc-pairs', 'coc-word']
    
    for l:ext in l:needed_extensions
        let l:found = 0
        for l:installed_ext in l:installed
            if l:installed_ext.id == l:ext
                let l:found = 1
                break
            endif
        endfor
        
        if !l:found
            echom 'Installing Coc extension: ' . l:ext
            execute 'CocInstall ' . l:ext
        endif
    endfor
endfunction

" Auto-install extensions on first Coc startup (with delay to ensure Coc is ready)
autocmd User CocNvimInit call timer_start(1000, {-> s:InstallCocExtensions()})

" Additional Coc settings for stability
" Disable Coc for certain file types to prevent conflicts
autocmd FileType help,fugitive,nerdtree,tagbar let b:coc_enabled = 0

" Ensure proper cleanup on Vim exit
autocmd VimLeave * if exists('g:did_coc_loaded') | call CocActionAsync('shutdown') | endif

" Final verification that updatetime is appropriate for Coc
if &updatetime > 1000
    echom 'Warning: updatetime is ' . &updatetime . 'ms, which may cause slow Coc responses. Consider setting it to 300ms.'
endif
