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
    autocmd FileType php nnoremap <buffer> <F6> :!php %<CR>

    " Language-specific omnicompletion
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
    autocmd FileType php setlocal omnifunc=phpcomplete#CompletePHP

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
" COC PHP-SPECIFIC CONFIGURATION
" ============================================================================

" Enhanced PHP LSP integration with PHP-CS-Fixer formatting
augroup coc_php
    autocmd!
    " Enable Coc for PHP files
    autocmd FileType php let b:coc_suggest_disable = 0
    autocmd FileType php let b:coc_diagnostic_disable = 0

    " PHP will use the existing global CoC keymaps from completion.vim
    " No need for duplicate PHP-specific mappings
augroup END

" Enhanced PHP auto-formatting function that combines LSP and PHP-CS-Fixer
function! s:PHPAutoFormat() abort
    " First organize imports via LSP if available
    if exists('g:did_coc_loaded') && coc#rpc#ready()
        call <SID>SafeCocActionAsync('runCommand', 'editor.action.organizeImport')
        " Small delay to ensure imports are organized before PHP-CS-Fixer formatting
        call timer_start(100, {-> s:FormatWithPHPCSFixer()})
        echo 'Auto-formatting: imports organized, applying PHP-CS-Fixer...'
    else
        " Fall back to just PHP-CS-Fixer formatting
        call s:FormatWithPHPCSFixer()
    endif
endfunction

" PHP documentation lookup function
function! s:PHPDocumentationLookup() abort
    let l:word = expand('<cword>')
    if empty(l:word)
        echo 'No word under cursor for documentation lookup.'
        return
    endif

    " Try CoC hover first
    if exists('g:did_coc_loaded') && coc#rpc#ready()
        call <SID>SafeCocActionAsync('doHover')
    else
        " Fall back to online PHP documentation
        let l:php_doc_url = 'https://www.php.net/manual/en/function.' . substitute(tolower(l:word), '_', '-', 'g') . '.php'
        if executable('xdg-open')
            call system('xdg-open ' . shellescape(l:php_doc_url) . ' &')
        elseif executable('open')
            call system('open ' . shellescape(l:php_doc_url) . ' &')
        else
            echo 'Documentation URL: ' . l:php_doc_url
        endif
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

" C file setup with crash protection (no manual keymaps needed)
function! s:SetupCPasteSafety() abort
    " Automatic paste protection via Ctrl+v mapping, no manual commands needed
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

" ============================================================================
" PHP-SPECIFIC CONFIGURATION
" ============================================================================

" PHP configuration variables
let g:php_cs_fixer_enabled = 1
let g:php_cs_fixer_check_performed = 0
let g:php_cs_fixer_available = 0
let g:php_paste_optimized = 0
let g:php_paste_safe_mode = 0
let g:php_paste_max_lines = 100

" PHP syntax enhancements
let g:php_syntax_extensions_enabled = [
    \ "bcmath", "bz2", "core", "curl", "date", "dom", "ereg", "gd", "gettext",
    \ "hash", "iconv", "json", "libxml", "mbstring", "mcrypt", "mhash", "mysql",
    \ "mysqli", "openssl", "pcre", "pdo", "posix", "session", "simplexml",
    \ "sockets", "spl", "sqlite", "standard", "tokenizer", "wddx", "xml",
    \ "xmlreader", "xmlwriter", "zip", "zlib"
\ ]

" Enhanced PHP indentation and formatting
let g:php_html_load = 1
let g:php_html_in_heredoc = 1
let g:php_html_in_nowdoc = 1
let g:php_sql_query = 1
let g:php_sql_heredoc = 1
let g:php_sql_nowdoc = 1

" PSR standard configuration
let g:php_cs_fixer_rules = "@PSR12"
let g:php_cs_fixer_config_file = '.php_cs.dist'

" ============================================================================
" PHP AUTOCOMMANDS AND SETUP
" ============================================================================

augroup php_development
    autocmd!
    " Basic PHP settings
    autocmd FileType php setlocal tabstop=4 shiftwidth=4 expandtab
    autocmd FileType php setlocal autoindent smartindent cindent
    autocmd FileType php setlocal number ruler showcmd tw=120 cc=121
    autocmd FileType php setlocal foldmethod=indent foldlevelstart=99

    " PHP-specific syntax settings
    autocmd FileType php setlocal iskeyword+=$
    autocmd FileType php setlocal includeexpr=substitute(v:fname,'\\.','/','g')
    autocmd FileType php setlocal suffixesadd=.php

    " Enhanced PHP matchpairs for better bracket matching
    autocmd FileType php setlocal matchpairs+=<:>

    " PHP file type detection enhancements
    autocmd BufRead,BufNewFile *.phtml setfiletype php
    autocmd BufRead,BufNewFile *.php3 setfiletype php
    autocmd BufRead,BufNewFile *.php4 setfiletype php
    autocmd BufRead,BufNewFile *.php5 setfiletype php
    autocmd BufRead,BufNewFile *.phps setfiletype php

    " Initialize PHP-specific features
    autocmd FileType php call <SID>SetupPHPPaste()
    autocmd FileType php call <SID>SetupPHPIndentation()

    " PHP auto-formatting with PHP-CS-Fixer
    autocmd BufWritePre *.php call <SID>FormatWithPHPCSFixer()

    " PHP template and snippet setup
    autocmd FileType php call <SID>SetupPHPTemplates()

    " PHP debugging and development tools
    autocmd FileType php nnoremap <buffer> <F7> :call <SID>PHPLint()<CR>
    autocmd FileType php nnoremap <buffer> <F8> :call <SID>PHPSyntaxCheck()<CR>
augroup END

" ============================================================================
" PHP PASTE SAFETY AND OPTIMIZATION
" ============================================================================

function! s:SetupPHPPaste() abort
    if g:php_paste_optimized | return | endif
    let g:php_paste_optimized = 1

    " PHP indentation optimizations
    setlocal autoindent smartindent
    setlocal indentkeys=0{,0},0),0],0#,!^F,o,O,e
    setlocal indentkeys+=0=?>,0=</,0=>

    " Use F-keys for PHP development (no leader conflicts)
    nnoremap <buffer><silent> <F9> :call <SID>FormatCurrentFileWithPHPCSFixer()<CR>
    nnoremap <buffer><silent> <F10> :call <SID>PHPDebugToggle()<CR>
endfunction

" Safe PHP paste detection and execution
function! s:PHPPasteBelow() abort
    let l:clipboard = @+
    if len(split(l:clipboard, '\n')) > g:php_paste_max_lines || l:clipboard =~# '<?php\|class\s\+\w\+\|namespace\s\+\w\+'
        call s:EnterPHPPasteSafeMode()
        silent normal! o
        silent put +
        call timer_start(200, {-> s:ExitPHPPasteSafeMode()})
    else
        set paste | normal! o | silent put + | set nopaste
        call s:IndentPastedPHPLines(line('.') - len(split(l:clipboard, '\n')), line('.'))
    endif
endfunction

function! s:PHPPasteAbove() abort
    let l:start_line = line('.')
    set paste
    normal! O
    silent put! +
    set nopaste
    call s:IndentPastedPHPLines(line('.'), l:start_line)
endfunction

" Enter safe paste mode for PHP files
function! s:EnterPHPPasteSafeMode() abort
    let g:php_paste_safe_mode = 1
    let b:coc_enabled = 0
    let b:coc_suggest_disable = 1
    let b:ale_enabled = 0
    set paste updatetime=2000 lazyredraw
    if exists('g:did_coc_loaded') && coc#rpc#ready()
        try | call CocActionAsync('clearHighlight') | catch | endtry
    endif
    echo 'PHP Safe Paste Mode: ON'
endfunction

" Exit safe paste mode
function! s:ExitPHPPasteSafeMode() abort
    let g:php_paste_safe_mode = 0
    let b:coc_enabled = 1
    let b:coc_suggest_disable = 0
    let b:ale_enabled = 1
    set nopaste updatetime=300 nolazyredraw
    echo 'PHP Safe Paste Mode: OFF'
endfunction

" Smart indentation limited to paste area for PHP
function! s:IndentPastedPHPLines(start, end) abort
    let l:line_count = a:end - a:start + 1
    if l:line_count > 50
        echo 'Large PHP paste detected (' . l:line_count . ' lines). Skipping auto-indent for performance.'
        return
    endif
    execute a:start . ',' . a:end . 'normal! =='
endfunction

" Limited scope indentation fix for performance
function! s:FixPHPIndentLimited() abort
    let l:total_lines = line('$')
    if l:total_lines > 1000
        let l:choice = confirm('Large PHP file (' . l:total_lines . ' lines). Indent entire file?', "&Yes\n&No\n&Current function only", 2)
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
" PHP INDENTATION SETUP
" ============================================================================

function! s:SetupPHPIndentation() abort
    " Enhanced PHP indentation settings
    setlocal cinkeys=0{,0},0),0],0#,!^F,o,O,e
    setlocal cinkeys+=0=?>,0=</,0=>
    setlocal cinoptions=l1,p0,)50,*50,t0,{1s,>2s,n-1s,:0,=1s

    " PHP-specific indent expressions
    setlocal indentexpr=GetPhpIndent()
    setlocal indentkeys=0{,0},0),0],0#,!^F,o,O,e,0=?>,0=</,0=>
endfunction

" ============================================================================
" PHP-CS-FIXER INTEGRATION
" ============================================================================

" Check PHP-CS-Fixer availability once per session
function! s:CheckPHPCSFixerAvailability() abort
    if g:php_cs_fixer_check_performed | return g:php_cs_fixer_available | endif
    let g:php_cs_fixer_check_performed = 1

    if !executable('php-cs-fixer')
        let g:php_cs_fixer_available = 0
        echo 'PHP-CS-Fixer not found in PATH. PHP auto-formatting disabled.'
        return 0
    endif

    let g:php_cs_fixer_available = 1
    return 1
endfunction

" Format current buffer with PHP-CS-Fixer
function! s:FormatWithPHPCSFixer() abort
    " Check if auto-formatting is enabled
    if !g:php_cs_fixer_enabled | return | endif

    " Check PHP-CS-Fixer availability (cached after first check)
    if !s:CheckPHPCSFixerAvailability() | return | endif

    " Save cursor position and view
    let l:view = winsaveview()
    let l:cursor_line = line('.')
    let l:cursor_col = col('.')

    " Get current buffer content
    let l:original_content = join(getline(1, '$'), "\n")

    " Create temporary file for PHP-CS-Fixer processing
    let l:temp_file = tempname() . '.php'
    call writefile(split(l:original_content, "\n"), l:temp_file)

    " Run PHP-CS-Fixer on temporary file
    let l:php_cs_fixer_cmd = 'php-cs-fixer fix --rules=' . shellescape(g:php_cs_fixer_rules) . ' ' . shellescape(l:temp_file) . ' 2>&1'
    let l:result = system(l:php_cs_fixer_cmd)
    let l:exit_code = v:shell_error

    if l:exit_code == 0
        " PHP-CS-Fixer succeeded, read formatted content
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

            echo 'PHP code formatted with PHP-CS-Fixer (' . g:php_cs_fixer_rules . ')'
        endif
    else
        " PHP-CS-Fixer failed, show error message
        let l:error_msg = substitute(l:result, '\n.*', '', '')
        echo 'PHP-CS-Fixer formatting failed: ' . l:error_msg
    endif

    " Clean up temporary file
    call delete(l:temp_file)
endfunction

" Toggle PHP-CS-Fixer auto-formatting
function! s:TogglePHPCSFixerFormatting() abort
    let g:php_cs_fixer_enabled = !g:php_cs_fixer_enabled
    let l:status = g:php_cs_fixer_enabled ? 'enabled' : 'disabled'
    echo 'PHP-CS-Fixer auto-formatting ' . l:status
endfunction

" Manual PHP-CS-Fixer formatting command
function! s:FormatCurrentFileWithPHPCSFixer() abort
    if !s:CheckPHPCSFixerAvailability()
        echo 'PHP-CS-Fixer is not available. Please install PHP-CS-Fixer: composer global require friendsofphp/php-cs-fixer'
        return
    endif
    call s:FormatWithPHPCSFixer()
endfunction

" ============================================================================
" PHP DEVELOPMENT TOOLS
" ============================================================================

" PHP syntax checking
function! s:PHPSyntaxCheck() abort
    if !executable('php')
        echo 'PHP is not available for syntax checking.'
        return
    endif

    let l:current_file = expand('%:p')
    if empty(l:current_file)
        echo 'No file to check syntax for.'
        return
    endif

    let l:result = system('php -l ' . shellescape(l:current_file))
    let l:exit_code = v:shell_error

    if l:exit_code == 0
        echo 'PHP syntax OK'
    else
        echo 'PHP syntax error: ' . substitute(l:result, '\n', ' ', 'g')
    endif
endfunction

" PHP linting
function! s:PHPLint() abort
    if !executable('phpcs')
        echo 'PHPCS not found. Install with: composer global require squizlabs/php_codesniffer'
        return
    endif

    let l:current_file = expand('%:p')
    if empty(l:current_file)
        echo 'No file to lint.'
        return
    endif

    let l:result = system('phpcs --standard=PSR12 ' . shellescape(l:current_file))
    let l:exit_code = v:shell_error

    if l:exit_code == 0
        echo 'PHP code style OK (PSR-12)'
    else
        echo 'PHP linting issues found. Check :copen for details.'
        cexpr l:result
        copen
    endif
endfunction

" PHP debugging toggle
function! s:PHPDebugToggle() abort
    if exists('b:php_debug_enabled') && b:php_debug_enabled
        let b:php_debug_enabled = 0
        echo 'PHP debug mode OFF'
    else
        let b:php_debug_enabled = 1
        echo 'PHP debug mode ON'
    endif
endfunction

" ============================================================================
" PHP TEMPLATE AND SNIPPET SETUP
" ============================================================================

function! s:SetupPHPTemplates() abort
    " PHP tag mappings for quick insertion
    inoremap <buffer> <?p <?php<space>
    inoremap <buffer> <?= <?=<space><space>?><left><left><left>
    inoremap <buffer> <? <?php<space>

    " PHP class template
    inoremap <buffer> phpcls <C-o>:call <SID>InsertPHPClass()<CR>

    " PHP function template
    inoremap <buffer> phpfun <C-o>:call <SID>InsertPHPFunction()<CR>

    " PHP namespace template
    inoremap <buffer> phpns <C-o>:call <SID>InsertPHPNamespace()<CR>

    " PHP use statement template
    inoremap <buffer> phpuse <C-o>:call <SID>InsertPHPUse()<CR>
endfunction

" Insert PHP class template
function! s:InsertPHPClass() abort
    let l:class_name = input('Class name: ')
    if !empty(l:class_name)
        let l:template = [
            \ '<?php',
            \ '',
            \ 'class ' . l:class_name,
            \ '{',
            \ '    public function __construct()',
            \ '    {',
            \ '        ',
            \ '    }',
            \ '}'
        \ ]
        call append(line('.'), l:template)
        normal! 7j$
    endif
endfunction

" Insert PHP function template
function! s:InsertPHPFunction() abort
    let l:function_name = input('Function name: ')
    if !empty(l:function_name)
        let l:template = [
            \ 'public function ' . l:function_name . '()',
            \ '{',
            \ '    ',
            \ '}'
        \ ]
        call append(line('.'), l:template)
        normal! 2j$
    endif
endfunction

" Insert PHP namespace template
function! s:InsertPHPNamespace() abort
    let l:namespace = input('Namespace: ')
    if !empty(l:namespace)
        call append(line('.'), 'namespace ' . l:namespace . ';')
        normal! j$
    endif
endfunction

" Insert PHP use statement template
function! s:InsertPHPUse() abort
    let l:use_class = input('Use class: ')
    if !empty(l:use_class)
        call append(line('.'), 'use ' . l:use_class . ';')
        normal! j$
    endif
endfunction

" ============================================================================
" PHP COMMANDS
" ============================================================================

" Commands for manual PHP-CS-Fixer control
command! PHPCSFixerFix call <SID>FormatCurrentFileWithPHPCSFixer()
command! PHPCSFixerToggle call <SID>TogglePHPCSFixerFormatting()
command! PHPCSFixerFixFile call <SID>FormatCurrentFileWithPHPCSFixer()

" PHP development commands
command! PHPSyntaxCheck call <SID>PHPSyntaxCheck()
command! PHPLint call <SID>PHPLint()
command! PHPDebugToggle call <SID>PHPDebugToggle()

" PHP paste commands
command! PHPPasteToggle call <SID>TogglePHPPasteSafeMode()
command! PHPPasteStatus echo 'PHP Safe Paste: ' . (g:php_paste_safe_mode ? 'ON' : 'OFF')

function! s:TogglePHPPasteSafeMode() abort
    if g:php_paste_safe_mode | call s:ExitPHPPasteSafeMode() | else | call s:EnterPHPPasteSafeMode() | endif
endfunction