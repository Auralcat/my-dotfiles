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

" ============================================================================
" PHP-SPECIFIC KEYMAPS
" ============================================================================

augroup php_keymaps
    autocmd!
    " PHP file switching and navigation
    autocmd FileType php nnoremap <buffer> <leader>fh :call <SID>SwitchToRelatedPHPFile('html')<CR>
    autocmd FileType php nnoremap <buffer> <leader>fc :call <SID>SwitchToRelatedPHPFile('css')<CR>
    autocmd FileType php nnoremap <buffer> <leader>fj :call <SID>SwitchToRelatedPHPFile('js')<CR>
    autocmd FileType php nnoremap <buffer> <leader>ft :call <SID>SwitchToRelatedPHPFile('twig')<CR>

    " Quick PHP echo/print/var_dump shortcuts
    autocmd FileType php nnoremap <buffer> <leader>pe oecho "";<Esc>2hi
    autocmd FileType php nnoremap <buffer> <leader>pv ovar_dump();<Esc>2hi
    autocmd FileType php nnoremap <buffer> <leader>pp oprint_r();<Esc>2hi
    autocmd FileType php nnoremap <buffer> <leader>pd odie();<Esc>2hi

    " PHP array and object shortcuts
    autocmd FileType php inoremap <buffer> $$ $this->
    autocmd FileType php inoremap <buffer> :: ::
    autocmd FileType php inoremap <buffer> => <space>=><space>

    " PHP block comments
    autocmd FileType php nnoremap <buffer> <leader>/* O/**<CR>/<Esc>O<space>
    autocmd FileType php nnoremap <buffer> <leader>*/ A<space>*/<Esc>

    " Quick PHP tag insertion for mixed HTML/PHP
    autocmd FileType php inoremap <buffer> <leader><? <?php<space>
    autocmd FileType php inoremap <buffer> <leader>?> <space>?>

    " PHP method visibility shortcuts
    autocmd FileType php inoremap <buffer> pubf public function<space>
    autocmd FileType php inoremap <buffer> prif private function<space>
    autocmd FileType php inoremap <buffer> prof protected function<space>

    " Quick return statements
    autocmd FileType php inoremap <buffer> ret return<space>
    autocmd FileType php inoremap <buffer> rett return true;
    autocmd FileType php inoremap <buffer> retf return false;
    autocmd FileType php inoremap <buffer> retn return null;

    " Exception handling shortcuts
    autocmd FileType php inoremap <buffer> tryy try {<CR>} catch (Exception $e) {<CR>}<Esc>2ko<tab>
    autocmd FileType php inoremap <buffer> throww throw new Exception('');<Esc>2hi

    " PSR-4 autoloading and namespace helpers
    autocmd FileType php nnoremap <buffer> <leader>ns :call <SID>InsertPSRNamespace()<CR>
    autocmd FileType php nnoremap <buffer> <leader>cl :call <SID>InsertPSRClass()<CR>
augroup END

" ============================================================================
" PHP FILE SWITCHING FUNCTIONS
" ============================================================================

" Switch to related files (CSS, JS, HTML, Twig templates)
function! s:SwitchToRelatedPHPFile(extension) abort
    let l:current_file = expand('%:r')
    let l:target_file = l:current_file . '.' . a:extension

    " Try different common locations for related files
    let l:search_paths = [
        \ l:target_file,
        \ 'templates/' . l:target_file,
        \ 'views/' . l:target_file,
        \ 'public/' . l:target_file,
        \ 'assets/' . l:target_file,
        \ 'resources/views/' . l:target_file,
        \ 'src/views/' . l:target_file
    \ ]

    for l:path in l:search_paths
        if filereadable(l:path)
            execute 'edit ' . l:path
            return
        endif
    endfor

    " If file doesn't exist, ask user if they want to create it
    let l:choice = confirm('Related file not found. Create ' . l:target_file . '?', "&Yes\n&No", 2)
    if l:choice == 1
        execute 'edit ' . l:target_file
    endif
endfunction

" ============================================================================
" PSR-4 NAMESPACE AND CLASS HELPERS
" ============================================================================

" Insert PSR-4 compliant namespace based on directory structure
function! s:InsertPSRNamespace() abort
    let l:file_path = expand('%:p:h')
    let l:project_root = finddir('.git/..', l:file_path . ';')

    if empty(l:project_root)
        let l:namespace = input('Namespace: ')
    else
        " Try to determine namespace from path
        let l:relative_path = substitute(l:file_path, l:project_root . '/', '', '')
        let l:namespace_parts = split(l:relative_path, '/')

        " Common PSR-4 transformations
        if !empty(l:namespace_parts) && l:namespace_parts[0] ==# 'src'
            call remove(l:namespace_parts, 0)
        endif

        let l:namespace = join(map(l:namespace_parts, 'substitute(v:val, "^.", "\\u&", "")'), '\\')
        let l:namespace = input('Namespace (detected): ', l:namespace)
    endif

    if !empty(l:namespace)
        call append(0, '<?php')
        call append(1, '')
        call append(2, 'namespace ' . l:namespace . ';')
        call append(3, '')
        normal! 5G
    endif
endfunction

" Insert PSR-4 compliant class with namespace
function! s:InsertPSRClass() abort
    let l:class_name = expand('%:t:r')
    let l:class_name = substitute(l:class_name, '^\w', '\u&', '')
    let l:class_name = input('Class name: ', l:class_name)

    if !empty(l:class_name)
        call s:InsertPSRNamespace()

        let l:template = [
            \ 'class ' . l:class_name,
            \ '{',
            \ '    public function __construct()',
            \ '    {',
            \ '        ',
            \ '    }',
            \ '}'
        \ ]

        call append(line('.'), l:template)
        normal! 2j$
    endif
endfunction