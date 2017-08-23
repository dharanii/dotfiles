" Plug {{{
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

Plug 'dylanaraps/wal.vim'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'kien/ctrlp.vim'

Plug 'neovimhaskell/haskell-vim'
Plug 'ElmCast/elm-vim'

call plug#end()

autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
" }}}

" Option {{{
" indent settings
set smartindent
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set shiftround

" better navigation
" set ignorecase
" set hlsearch
" set incsearch
" set smartcase

" misc settings
set hidden

" display settings
set number
set noshowmode
" }}}


" Color {{{1
" set background=dark
colorscheme wal 
" }}}1


" Plugin {{{
" vim-airline/vim-airline {{{
let g:airline_theme='minimalist'
let g:airline_powerline_fonts = 1
" }}}


" Filetype {{{1
augroup vimrc
  " Filetype: Haskell {{{2
  autocmd FileType haskell setlocal sts=4 sw=4
  " Filetype: Elm {{{2
  autocmd FileType elm setlocal sts=4 sw=4
" }}}2
augroup END
" }}}1

