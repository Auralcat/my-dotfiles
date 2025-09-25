" ============================================================================
" COMPLETION AND LSP CONFIGURATION
" ============================================================================
" CoC.nvim and completion-related configuration
" File: vim/config/completion.vim

" ============================================================================
" SAFE COC WRAPPER FUNCTIONS
" ============================================================================

" Helper function to check if CoC is available and filetype is supported
function! s:IsCocSupported() abort
    if !exists('g:did_coc_loaded') || !coc#rpc#ready()
        return 0
    endif

    " Define supported file types for CoC
    let l:supported_types = ['python', 'javascript', 'typescript', 'c', 'cpp', 'java', 'ruby', 'css', 'html', 'json', 'yaml', 'xml', 'go', 'rust', 'php', 'vim']
    return index(l:supported_types, &filetype) >= 0
endfunction

" Safe wrapper for coc#refresh()
function! s:SafeCocRefresh() abort
    if s:IsCocSupported()
        try
            return coc#refresh()
        catch
            return "\<C-n>"
        endtry
    endif
    return "\<C-n>"
endfunction

" Safe wrapper for coc#float#has_scroll()
function! s:SafeCocFloatHasScroll() abort
    if s:IsCocSupported()
        try
            return coc#float#has_scroll()
        catch
            return 0
        endtry
    endif
    return 0
endfunction

" Safe wrapper for coc#float#scroll()
function! s:SafeCocFloatScroll(forward) abort
    if s:IsCocSupported()
        try
            return coc#float#scroll(a:forward)
        catch
            return a:forward ? "\<C-f>" : "\<C-b>"
        endtry
    endif
    return a:forward ? "\<C-f>" : "\<C-b>"
endfunction

" ============================================================================
" COC.NVIM CORE CONFIGURATION
" ============================================================================

" Use tab for trigger completion with characters ahead and navigate
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ <SID>SafeCocRefresh()
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

" ============================================================================
" COC NAVIGATION AND DIAGNOSTICS
" ============================================================================

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

" ============================================================================
" COC REFACTORING AND CODE ACTIONS
" ============================================================================

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

" ============================================================================
" COC SCROLL SUPPORT
" ============================================================================

" Remap <C-f> and <C-b> for scroll float windows/popups
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> <SID>SafeCocFloatHasScroll() ? <SID>SafeCocFloatScroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> <SID>SafeCocFloatHasScroll() ? <SID>SafeCocFloatScroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> <SID>SafeCocFloatHasScroll() ? "\<C-r>=".escape(<SID>SafeCocFloatScroll(1), '"')."\<CR>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> <SID>SafeCocFloatHasScroll() ? "\<C-r>=".escape(<SID>SafeCocFloatScroll(0), '"')."\<CR>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> <SID>SafeCocFloatHasScroll() ? <SID>SafeCocFloatScroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> <SID>SafeCocFloatHasScroll() ? <SID>SafeCocFloatScroll(0) : "\<C-b>"
endif

" Use CTRL-S for selections ranges
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" ============================================================================
" COC COMMANDS
" ============================================================================

" Add `:Format` command to format current buffer
command! -nargs=0 Format :call <SID>SafeCocActionAsync('format')

" Add `:Fold` command to fold current buffer
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer
command! -nargs=0 OR :call <SID>SafeCocActionAsync('runCommand', 'editor.action.organizeImport')

" ============================================================================
" COC LIST MAPPINGS
" ============================================================================

" Mappings for CoCList
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<CR>
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<CR>
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<CR>
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<CR>
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<CR>
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

" ============================================================================
" COC CURSORHOLD CONFIGURATION
" ============================================================================

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

" ============================================================================
" COC STATUSLINE CONFIGURATION
" ============================================================================

" Conditional statusline that only calls CoC functions when CoC is ready
function! s:SafeCocStatus() abort
    " Only call coc#status() if CoC is loaded and ready
    if exists('g:did_coc_loaded') && coc#rpc#ready()
        try
            return coc#status()
        catch
            return ''
        endtry
    endif
    return ''
endfunction

function! s:SafeCocFunction() abort
    " Only get current function if CoC is ready and buffer supports it
    if exists('g:did_coc_loaded') && coc#rpc#ready() && get(b:, 'coc_enabled', 1)
        return get(b:, 'coc_current_function', '')
    endif
    return ''
endfunction

" Set conditional statusline that won't cause startup errors
augroup safe_coc_statusline
    autocmd!
    " Only enable statusline for programming file types where CoC is useful
    autocmd FileType python,javascript,typescript,c,cpp,java,ruby,css,html,json,yaml,xml,go,rust,php,vim
          \ setlocal statusline^=%{<SID>SafeCocStatus()}%{<SID>SafeCocFunction()}
augroup END

" ============================================================================
" COC INITIALIZATION AND ERROR HANDLING
" ============================================================================

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

" ============================================================================
" COC ERROR HANDLING UTILITIES
" ============================================================================

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

" ============================================================================
" COC EXTENSIONS AUTO-INSTALL
" ============================================================================

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
autocmd User CocNvimInit if &filetype =~# 'python\|javascript\|typescript\|c\|cpp\|java\|ruby\|css\|html\|json\|yaml\|xml\|go\|rust\|php\|vim' |
      \ call timer_start(1000, {-> s:InstallCocExtensions()}) | endif

" ============================================================================
" COC CLEANUP
" ============================================================================

" Ensure proper cleanup on Vim exit
autocmd VimLeave * if exists('g:did_coc_loaded') | call CocActionAsync('shutdown') | endif