" ============================================================================
" WRITING-FOCUSED CONFIGURATION
" ============================================================================
" VimPencil and writing-focused settings, CoC disabling for writing modes
" File: vim/config/writing.vim

" ============================================================================
" VIM PENCIL CONFIGURATION
" ============================================================================

" VimPencil settings for enhanced writing experience
augroup pencil
    autocmd!
    " Enable pencil for markdown files
    autocmd FileType markdown if exists('*pencil#init') | call pencil#init() | endif
    " Enable pencil for text files with soft wrap
    autocmd FileType text if exists('*pencil#init') | call pencil#init({'wrap': 'soft'}) | endif
augroup END

" ============================================================================
" TEXT MODE SETTINGS
" ============================================================================

" Text Mode configuration for distraction-free writing
augroup writing
    autocmd!
    " Disable line numbers for text files
    autocmd FileType text set nonumber
    " Disable ruler for text files
    autocmd FileType text set noruler
    " Enable SoftPencil if available
    autocmd FileType text if exists(':SoftPencil') | SoftPencil | endif
augroup END

" ============================================================================
" WRITING-FOCUSED COC DISABLING
" ============================================================================

" Define writing-focused file types that should have CoC disabled
let g:coc_writing_filetypes = [
  \ 'text', 'markdown', 'md', 'txt', 'rst', 'org', 'tex', 'latex',
  \ 'asciidoc', 'wiki', 'confluence', 'gitcommit', 'COMMIT_EDITMSG',
  \ 'mail', 'notes', 'journal', 'diary'
  \ ]

" Define special buffer types that should have CoC disabled
let g:coc_disabled_special_buffers = [
  \ 'help', 'fugitive', 'nerdtree', 'tagbar', 'quickfix', 'loclist',
  \ 'netrw', 'startify', 'dashboard', 'alpha'
  \ ]

" Comprehensive CoC disabling function
function! s:DisableCocForWriting() abort
    " Disable all major CoC features for writing-focused files
    let b:coc_enabled = 0
    let b:coc_suggest_disable = 1
    let b:coc_diagnostic_disable = 1

    " Disable specific CoC features that might interfere with writing
    if exists('b:coc_pairs_disabled')
        let b:coc_pairs_disabled = ['<', '>', '"', "'"]
    endif

    " Disable semantic highlighting for writing files
    if exists('*CocAction')
        try
            call CocAction('clearHighlight')
        catch
            " Silently handle any errors
        endtry
    endif
endfunction

" Apply CoC disabling to writing and special file types
augroup coc_writing_disable
    autocmd!
    " Disable for writing-focused file types
    for filetype in g:coc_writing_filetypes
        execute 'autocmd FileType ' . filetype . ' call <SID>DisableCocForWriting()'
    endfor

    " Disable for special buffer types
    for filetype in g:coc_disabled_special_buffers
        execute 'autocmd FileType ' . filetype . ' let b:coc_enabled = 0'
    endfor

    " Additional buffer-type based disabling
    autocmd BufNewFile,BufRead *.{diary,journal,note,notes} call <SID>DisableCocForWriting()
    autocmd BufNewFile,BufRead {TODO,CHANGELOG,README,INSTALL,NEWS,COPYING}* call <SID>DisableCocForWriting()

    " Automatically disable CoC when entering VimPencil mode
    autocmd User PencilOn call <SID>DisableCocForWriting()
augroup END

" ============================================================================
" MANUAL COC CONTROL FOR WRITING
" ============================================================================

" Function to enable CoC for current buffer (override writing mode)
function! s:EnableCocForBuffer() abort
    let b:coc_enabled = 1
    let b:coc_suggest_disable = 0
    let b:coc_diagnostic_disable = 0

    " Re-enable CoC features
    if exists('b:coc_pairs_disabled')
        unlet b:coc_pairs_disabled
    endif

    echo 'CoC enabled for current buffer (' . &filetype . ')'
endfunction

" Function to disable CoC for current buffer
function! s:DisableCocForBuffer() abort
    call <SID>DisableCocForWriting()
    echo 'CoC disabled for current buffer (' . &filetype . ')'
endfunction

" Function to toggle CoC for current buffer
function! s:ToggleCocForBuffer() abort
    if get(b:, 'coc_enabled', 1)
        call <SID>DisableCocForBuffer()
    else
        call <SID>EnableCocForBuffer()
    endif
endfunction

" Check if current buffer is in writing mode
function! s:IsWritingBuffer() abort
    let l:writing_types = g:coc_writing_filetypes
    return index(l:writing_types, &filetype) >= 0
endfunction

" Smart CoC toggle that considers file type
function! s:SmartCocToggle() abort
    if <SID>IsWritingBuffer()
        call <SID>ToggleCocForBuffer()
    else
        echo 'CoC toggle only available for writing file types. Current: ' . &filetype
    endif
endfunction

" Function to show CoC status for current buffer
function! s:ShowCocStatus() abort
    let l:coc_enabled = get(b:, 'coc_enabled', 1)
    let l:suggest_disabled = get(b:, 'coc_suggest_disable', 0)
    let l:diagnostic_disabled = get(b:, 'coc_diagnostic_disable', 0)
    let l:is_writing_type = <SID>IsWritingBuffer()

    echo 'CoC Status for ' . &filetype . ':'
    echo '  Enabled: ' . (l:coc_enabled ? 'Yes' : 'No')
    echo '  Suggestions: ' . (l:suggest_disabled ? 'Disabled' : 'Enabled')
    echo '  Diagnostics: ' . (l:diagnostic_disabled ? 'Disabled' : 'Enabled')
    echo '  Writing type: ' . (l:is_writing_type ? 'Yes' : 'No')

    if exists('g:did_coc_loaded')
        echo '  CoC loaded: Yes'
        echo '  RPC ready: ' . (coc#rpc#ready() ? 'Yes' : 'No')
    else
        echo '  CoC loaded: No'
    endif
endfunction

" ============================================================================
" WRITING COMMANDS
" ============================================================================

" Commands for manual CoC control
command! CocEnable call <SID>EnableCocForBuffer()
command! CocDisable call <SID>DisableCocForBuffer()
command! CocToggle call <SID>ToggleCocForBuffer()
command! CocWritingToggle call <SID>SmartCocToggle()
command! CocStatus call <SID>ShowCocStatus()

" ============================================================================
" WRITING KEY MAPPINGS
" ============================================================================

" Key mappings for CoC control (only in writing buffers)
augroup coc_writing_mappings
    autocmd!
    for filetype in g:coc_writing_filetypes
        execute 'autocmd FileType ' . filetype . ' nnoremap <buffer><silent> <leader>ct :CocToggle<CR>'
        execute 'autocmd FileType ' . filetype . ' nnoremap <buffer><silent> <leader>ce :CocEnable<CR>'
        execute 'autocmd FileType ' . filetype . ' nnoremap <buffer><silent> <leader>cd :CocDisable<CR>'
    endfor
augroup END