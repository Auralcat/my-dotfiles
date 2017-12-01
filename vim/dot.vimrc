" Arquivo de configuração inicial para o workshop de Vim.
" Autora: Miriam Retka (30/11/2017)
"
" -----------------------Instruções-de-instalação------------------------------
" Windows:
" - Renomeie este arquivo para _vimrc e coloque na pasta onde o Vim foi
"   instalado.
"
" Mac:
" - Renomeie este arquivo para .vimrc e coloque na pasta HOME.
"
" Linux:
" - Renomeie este arquivo para .vimrc e coloque na pasta HOME.
" -----------------------------------------------------------------------------

" Mostra os comandos incompletos no modo normal e visual
set showcmd

" Mostra a posição do cursor (linha, coluna) no arquivo
set ruler

" Mostra o título do arquivo na parte de baixo do editor
set title

" Mostra resultados da busca usando ? ou / no modo normal à medida que você
" digitar o que quer procurar
set incsearch

" Coloca espaços no lugar de um caractere TAB. Ajuda a padronizar a
" apresentação do texto.
set expandtab

" Remove espaços em branco extras do fim das linhas quando salvar qualquer
" arquivo com texto (seja scripts ou plaintext)
autocmd BufWritePre * :%s/\s\+$//e
