" ============================================================================
" PROGRAMMING CONFIGURATION
" ============================================================================
" Language-specific programming settings, auto-formatting, and development tools
" File: vim/config/programming.vim

" ============================================================================
" OPTIMIZED PASTE MODE FOR PROGRAMMING
" ============================================================================

" Lazy-loaded paste functions to minimize startup overhead
let g:paste_optimized = 1

" Enhanced bracketed paste for modern terminals
if has('patch-8.0.0238') || has('nvim-0.2.3')
    try
        " Enable bracketed paste with proper escape sequences
        let &t_SI .= "\<Esc>[?2004h"
        let &t_EI .= "\<Esc>[?2004l"
        let &t_BE = "\<Esc>[?2004h"
    catch
        " Silently handle terminal compatibility issues
    endtry
endif

" ============================================================================
" CORE PROGRAMMING SETTINGS
" ============================================================================

" Core programming settings (consolidated for performance)
augroup programming
    autocmd!
    " Apply common programming settings
    autocmd FileType python,ruby,c,cpp,java,javascript,go,rust,php,typescript
        \ setlocal number ruler showcmd tw=79 cc=80
    " Enable rainbow parentheses if available
    autocmd FileType python,ruby,c,cpp,java,javascript,go,rust,php,typescript
        \ if exists(':RainbowParentheses') | RainbowParentheses | endif

    " Language-specific indentation
    autocmd FileType ruby setlocal tabstop=2 shiftwidth=2

    " Colorschemes for different languages
    autocmd FileType python colorscheme desert
    autocmd FileType ruby colorscheme landscape

    " Quick script execution mappings
    autocmd FileType python nnoremap <buffer> <F6> :!python3 %<CR>
    autocmd FileType ruby nnoremap <buffer> <F6> :!ruby %<CR>
    autocmd FileType javascript nnoremap <buffer> <F6> :!nodejs %<CR>

    " Language-specific omnicompletion
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete

    " Python-specific optimizations
    autocmd FileType python call <SID>SetupPythonPaste()
    autocmd FileType python setlocal cindent
    " Python auto-formatting with black
    autocmd BufWritePre *.py call <SID>FormatWithBlack()
augroup END

" ============================================================================
" PYTHON-SPECIFIC CONFIGURATION
" ============================================================================

" Python auto-formatting with black configuration
let g:python_black_enabled = 1
let g:python_black_check_performed = 0
let g:python_black_available = 0

" Lazy-loaded Python indentation functions
let g:python_paste_optimized = 0

function! s:SetupPythonPaste() abort
    if g:python_paste_optimized | return | endif
    let g:python_paste_optimized = 1

    " Python indentation handled by EditorConfig - only set smart indentation
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

" ============================================================================
" PYTHON BLACK AUTO-FORMATTING
" ============================================================================

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

" Commands for manual black control
command! BlackFormat call <SID>FormatCurrentFileWithBlack()
command! BlackToggle call <SID>ToggleBlackFormatting()

" ============================================================================
" PYTHON PASTE CLEANUP UTILITIES
" ============================================================================

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

" ============================================================================
" COC PYTHON-SPECIFIC CONFIGURATION
" ============================================================================

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

" Safe wrapper for CocActionAsync calls (referenced from completion.vim)
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

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    elseif (coc#rpc#ready())
        call <SID>SafeCocActionAsync('doHover')
    else
        execute '!' . &keywordprg . " " . expand('<cword>')
    endif
endfunction

" ============================================================================
" C/C++ PASTE CRASH PREVENTION SYSTEM - CRITICAL FIX
" ============================================================================

" Global protection state
let g:c_paste_safe_mode = 0
let g:c_paste_max_lines = 75

" Enter safe paste mode for C files
function! s:EnterCPasteSafeMode() abort
    let g:c_paste_safe_mode = 1
    let b:coc_enabled = 0
    let b:coc_suggest_disable = 1
    let b:ale_enabled = 0
    set paste updatetime=2000 lazyredraw
    if exists('g:did_coc_loaded') && coc#rpc#ready()
        try | call CocActionAsync('clearHighlight') | catch | endtry
    endif
    echo 'C Safe Paste Mode: ON'
endfunction

" Exit safe paste mode
function! s:ExitCPasteSafeMode() abort
    let g:c_paste_safe_mode = 0
    let b:coc_enabled = 1
    let b:coc_suggest_disable = 0
    let b:ale_enabled = 1
    set nopaste updatetime=300 nolazyredraw
    echo 'C Safe Paste Mode: OFF'
endfunction

" Safe paste detection and execution
function! s:CPasteBelow() abort
    let l:clipboard = @+
    if len(split(l:clipboard, '\n')) > g:c_paste_max_lines || l:clipboard =~# '#include\|struct\s\+\w\+\s*{\|typedef'
        call s:EnterCPasteSafeMode()
        silent normal! o
        silent put +
        call timer_start(200, {-> s:ExitCPasteSafeMode()})
    else
        set paste | normal! o | silent put + | set nopaste
    endif
endfunction

" C file setup with crash protection
function! s:SetupCPasteSafety() abort
    nnoremap <buffer><silent> <leader>cp :call <SID>CPasteBelow()<CR>
    nnoremap <buffer><silent> <leader>cs :call <SID>ToggleCPasteSafeMode()<CR>
endfunction

function! s:ToggleCPasteSafeMode() abort
    if g:c_paste_safe_mode | call s:ExitCPasteSafeMode() | else | call s:EnterCPasteSafeMode() | endif
endfunction

" Intercept insert mode paste
function! s:SafeInsertPaste() abort
    let l:clipboard = @+
    if len(split(l:clipboard, '\n')) > g:c_paste_max_lines || l:clipboard =~# '#include\|struct\s\+\w\+\s*{\|typedef'
        call s:EnterCPasteSafeMode()
        call timer_start(100, {-> feedkeys("\<C-r>+", 'n')})
        call timer_start(500, {-> s:ExitCPasteSafeMode()})
        return ""
    else
        return "\<C-r>+"
    endif
endfunction

" Auto-setup for C files
augroup c_paste_safety
    autocmd!
    autocmd FileType c,cpp call <SID>SetupCPasteSafety()
    autocmd FileType c,cpp inoremap <buffer><silent><expr> <C-v> <SID>SafeInsertPaste()
    autocmd InsertEnter *.c,*.cpp let b:coc_highlight_disabled = 1
    autocmd InsertLeave *.c,*.cpp let b:coc_highlight_disabled = 0
    autocmd BufLeave *.c,*.cpp call <SID>ExitCPasteSafeMode()
augroup END

" Override CoC CursorHold for C files
augroup c_coc_protection
    autocmd!
    autocmd FileType c,cpp autocmd CursorHold <buffer> call <SID>SafeCHighlight()
augroup END

function! s:SafeCHighlight() abort
    if !g:c_paste_safe_mode && !get(b:, 'coc_highlight_disabled', 0) && exists('g:did_coc_loaded') && coc#rpc#ready()
        try | call CocActionAsync('highlight') | catch | endtry
    endif
endfunction

" Commands
command! CPasteToggle call <SID>ToggleCPasteSafeMode()
command! CPasteStatus echo 'C Safe Paste: ' . (g:c_paste_safe_mode ? 'ON' : 'OFF')