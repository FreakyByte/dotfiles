""" Required for Vundle
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

""" Plugins
Plugin 'airblade/vim-gitgutter'
Plugin 'itchyny/lightline.vim'
Bundle 'sonph/onehalf', {'rtp': 'vim/'}
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-surround'
Plugin 'Valloric/YouCompleteMe'
Plugin 'lervag/vimtex'
Plugin 'svermeulen/vim-easyclip'
Plugin 'dylanaraps/wal.vim'
Plugin 'luochen1990/rainbow'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

""" Basic settings
set nu		" absolute line numbers
set rnu		" relative line numbers

set showcmd	"show partial commands in last line
set ignorecase	"ignore case when searching
set incsearch	"show the next match while entering a search

let mapleader = "," " map leader to comma
let maplocalleader = "," " map leader to comma

let g:rainbow_active = 1

"colorscheme wal

""" lightline
set laststatus=2
let g:lightline = {
      \ 'colorscheme': 'onehalfdark',
      \ }

""" latex-suite
:let Tex_FoldedSections=""
:let Tex_FoldedEnvironments=""
:let Tex_FoldedMisc=""                  "disable folding
let g:tex_flavor='latex'                "load latex-suite in empty .tex documents
autocmd FileType tex setlocal shiftwidth=2 tabstop=2 textwidth=100

"rebind vimtex's close delim so that it isn't annoying when typing ]] in R
autocmd VimEnter * silent! iunmap <buffer> ]]
"at VimEnter because otherwise it happens before the Plugin defines the map
"silent! because it otherwise gives error in other filetypes
autocmd VimEnter * imap <C-]> <Plug>(vimtex-delim-close)  
"disable indentation of &s
let g:vimtex_indent_on_ampersands = 0
""" YouCompleteMe
let g:ycm_path_to_python_interpreter = "/usr/bin/python3"

""" YouCompleteMe - vimtex - https://github.com/lervag/vimtex/blob/master/doc/vimtex.txt
if !exists('g:ycm_semantic_triggers')
   	   let g:ycm_semantic_triggers = {}
   endif

""" enable copying between vim instances
let g:EasyClipShareYanks=1
nmap M <Plug>MoveMotionEndOfLinePlug
"the above binds m to cut (delete and copy to clipboard)

