" ============================================================================
" KEY MAPPINGS AND SHORTCUTS
" ============================================================================
" All key bindings, mappings, and keyboard shortcuts
" File: vim/config/keymaps.vim

" ============================================================================
" PASTE MODE AND CLIPBOARD INTEGRATION
" ============================================================================

" Fast paste mode toggle
set pastetoggle=<F2>

" Fixes newline insertion problem in insert mode
inoremap <CR> <CR>

" Enhanced insert mode paste with proper mode handling
function! s:OptimizedInsertPaste() abort
    return "\<C-o>:set paste\<CR>\<C-r>+\<C-o>:set nopaste\<CR>"
endfunction

" Insert mode paste mappings
inoremap <expr> <C-v> <SID>OptimizedInsertPaste()
inoremap <expr> <C-S-v> <SID>OptimizedInsertPaste()
inoremap <expr> <C-S-V> <SID>OptimizedInsertPaste()

" Smart paste in normal mode
function! s:SmartPaste() abort
    let l:paste_state = &paste
    set paste
    execute 'normal! "+p'
    let &paste = l:paste_state
endfunction

" Normal mode paste mappings
nnoremap <silent> <leader>p :call <SID>SmartPaste()<CR>
nnoremap <silent> <leader>P :call <SID>SmartPaste()<CR>

" ============================================================================
" SYSTEM CLIPBOARD OPERATIONS
" ============================================================================

" Alternative clipboard paste mappings
nnoremap <leader>v "+p
nnoremap <leader>V "+P
vnoremap <leader>v "+p

" System clipboard copy mappings
nnoremap <leader>y "+y
vnoremap <leader>y "+y
nnoremap <leader>Y "+Y

" Copy whole file to system clipboard
nnoremap <leader>ya :%y+<CR>

" Primary selection (middle mouse) integration for Linux
if has('linux') || has('unix')
    nnoremap <leader>mp "*p
    nnoremap <leader>mP "*P
    nnoremap <leader>my "*y
    vnoremap <leader>my "*y
endif

" ============================================================================
" FILE OPERATIONS
" ============================================================================

" Quick file update/save
nnoremap <F5> :update<CR>

" ============================================================================
" WINDOW AND TAB NAVIGATION
" ============================================================================

" Quicker window navigation
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" Quicker window closing
" Note: Safe to use C-c since it shows 'please quit vim with :quit' if unbound
nnoremap <C-c> <C-w>q

" Tab navigation
noremap <leader>t :tabnew<CR>
noremap <leader>w :tabclose<CR>

" ============================================================================
" MOVEMENT RESTRICTIONS (Vim Hard Mode)
" ============================================================================

" Disable arrow keys in normal mode with helpful messages
map <up> :echoe "Please use k."<CR>
map <down> :echoe "Please use j."<CR>
map <left> :echoe "Please use h."<CR>
map <right> :echoe "Please use l."<CR>

" Disable arrow keys in insert mode (no echo in insert mode)
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" ============================================================================
" INSERT MODE EMACS BINDINGS
" ============================================================================

" Set Emacs bindings for insert mode
" Handy when typing autocompleted brackets/parentheses/quotes
imap <C-a> <C-o>0
imap <C-e> <C-o>$

" ============================================================================
" TODO AND TASK MANAGEMENT
" ============================================================================

" Todo management for .todo.md files
augroup todo_mappings
    autocmd!
    " Put a DONE stamp on the selected item
    autocmd BufReadPre .todo.md nnoremap <buffer> <C-i> ^wi*[DONE]*<space><C-[>]
augroup END