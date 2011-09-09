nmap <C-l> <Plug>(openbrowser-open)
" Vundle settings
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

" Define active plugins Vundle
Bundle 'fugitive.vim'
Bundle 'surround.vim'
Bundle 'ruby.vim'
Bundle 'eruby.vim'
Bundle 'rails.vim'
Bundle 'railscasts'
Bundle 'Align'
Bundle 'project.tar.gz'
Bundle 'ruby-matchit'
Bundle 'catn.vim'
Bundle 'unite.vim'
Bundle 'quickrun.vim'
"for quickrun outputter=browser
Bundle 'open-browser.vim'
Bundle 'Markdown'
Bundle 'snipMate'
Bundle 'TwitVim'

filetype plugin indent on
filetype off "here off, and after vundle finish, on again.

set encoding=utf8
set fileencoding=utf8
set autoindent
set nocursorline
set expandtab
set foldmethod=marker
set tags=./tags
set incsearch
set ignorecase
set laststatus=2
set number
set statusline=%F%r%m%h%w%=%l/%L(%3p%%)\ FileType:%y/Form:%{GetEFstatus()}
set shiftwidth=4
set tabstop=4
set visualbell
set nocompatible
set noswapfile
set nowrapscan
set helplang=ja,en
set splitbelow
set splitright
set gdefault " all substitution
set backupdir=$VIM/tmp,~,.
set paste

" cursorline settings
set cursorline

  augroup cch
    autocmd! cch
    autocmd WinLeave * set nocursorline
    autocmd WinEnter,BufRead * set cursorline
  augroup END

:hi clear CursorLine
:hi CursorLine gui=underline
highlight CursorLine ctermbg=black guibg=black

let g:vimproc_dll_path = "/home/hash/.vim/autoload/proc.so"

" Language Dictionaries
"set dictionary=$VIM/dict/java14.dict

" New undo-persistence feature of vim73
if has('persistent_undo')
    set undofile
    set undodir=./.undofiles,$VIM/.undofiles
endif

" FYI: how to check Vim version, extentions
" http://vim-users.jp/2010/01/hack115/

" automatically move to last line
autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

" automatically remove spaces end of the line
autocmd BufWritePre * :%s/\s\+$//ge

syntax on
" filetype on
filetype indent on
filetype plugin on

" omni completion setting
autocmd FileType *
\   if &l:omnifunc == ''
\ |   setlocal omnifunc=syntaxcomplete#Complete
\ | endif

" Rails settings
let g:rubycomplete_rails = 1

" Color Settings
" execute ':so $VIMRUNTIME/syntax/colortest.vim' to view sample colors
colorscheme desert "murphy, darkblue
highlight LineNr ctermfg=darkgray
highlight Visual ctermfg=darkblue ctermbg=grey
highlight VisualNOS ctermfg=darkblue ctermbg=grey
highlight StatusLine cterm=reverse,bold
highlight StatusLine ctermfg=green ctermbg=white
highlight StatusLineNC cterm=reverse
" Fold-colors
highlight Folded guibg=grey guifg=blue
highlight FoldColumn guibg=darkgrey guifg=white
" change statusline color in insert mode
autocmd InsertEnter * highlight StatusLine ctermfg=red
autocmd InsertLeave * highlight StatusLine ctermfg=green

" Automatic recognition of Encoding{{{1
if &encoding !=# 'utf-8'
  set encoding=japan
  set fileencoding=japan
endif
if has('iconv')
  let s:enc_euc = 'euc-jp'
  let s:enc_jis = 'iso-2022-jp'
"  " check iconv can proccess eucIJ-ms or not
  if iconv("\x87\x64\x87\x6a", 'cp932', 'eucjp-ms') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'eucjp-ms'
    let s:enc_jis = 'iso-2022-jp-3'
"  " check iconv can proccess JISX0213 or not
  elseif iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'euc-jisx0213'
    let s:enc_jis = 'iso-2022-jp-3'
  endif
"  " constract fileencodings
  if &encoding ==# 'utf-8'
    let s:fileencodings_default = &fileencodings
    let &fileencodings = s:enc_jis .','. s:enc_euc .',cp932'
    let &fileencodings = &fileencodings .','. s:fileencodings_default
    unlet s:fileencodings_default
  else
    let &fileencodings = &fileencodings .','. s:enc_jis
    set fileencodings+=utf-8,ucs-2le,ucs-2
    if &encoding =~# '^\(euc-jp\|euc-jisx0213\|eucjp-ms\)$'
      set fileencodings+=cp932
      set fileencodings-=euc-jp
      set fileencodings-=euc-jisx0213
      set fileencodings-=eucjp-ms
      let &encoding = s:enc_euc
      let &fileencoding = s:enc_euc
    else
      let &fileencodings = &fileencodings .','. s:enc_euc
    endif
  endif
"  " unlet vars
  unlet s:enc_euc
  unlet s:enc_jis
endif
"" if no Japanese, use encoding for fileencoding
if has('autocmd')
  function! AU_ReCheck_FENC()
    if &fileencoding =~# 'iso-2022-jp' && search("[^\x01-\x7e]", 'n') == 0
      let &fileencoding=&encoding
    endif
  endfunction
  autocmd BufReadPost * call AU_ReCheck_FENC()
endif
set fileformats=unix,dos,mac
if exists('&ambiwidth')
  set ambiwidth=double
endif
"}}}1


"ESC key
"inoremap <C-Space> <C-[>
"cnoremap <C-Space> <C-[>
"vnoremap <C-Space> <C-[>
inoremap <C-Space> <C-[>
cnoremap <C-Space> <C-[>
nnoremap <C-Space> <C-[>
nnoremap <Space> <C-[>
nnoremap <F1> <C-[>
inoremap <F1> <C-[>
" always reset iminsert to zero when leaving Insert mode.
inoremap <ESC> <ESC>:set iminsert=0<CR>

" Replace colon with semi-colon
nnoremap ; :
vnoremap ; :
"inoremap <C-j> <CR>

" move by one display line {{{
noremap j gj
noremap k gk
noremap 0 g0
noremap $ g$
noremap gj j
noremap gk k
noremap g0 0
noremap g$ $
"}}}
" hjkl move in insert mode
"inoremap <C-j> <DOWN>
"inoremap <C-k> <UP>
"inoremap <C-l> <RIGHT>
"inoremap <C-h> <LEFT>

nnoremap <C-a> 0
nnoremap <C-e> $

" autocomplete (, [, {, ", '
inoremap () ()<LEFT>
inoremap <> <><LEFT>
inoremap {} {}<LEFT>
inoremap [] []<LEFT>
inoremap '' ''<LEFT>
inoremap "" ""<LEFT>


" When searching, always move the cursor to center of window
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz


" go to another window
nnoremap <space>j <C-W>j
nnoremap <space>k <C-W>k
nnoremap <space>h <C-W>h
nnoremap <space>l <C-W>l
" move the window to another position
nnoremap <space>J <C-W>J
nnoremap <space>K <C-W>K
nnoremap <space>H <C-W>H
nnoremap <space>L <C-W>L
" window width/height modification
nnoremap <space>+ <C-W>5+
nnoremap <space>- <C-W>5-
nnoremap <space>> <C-W>10>
nnoremap <space>< <C-W>10<
" execute current window using QuickRun
nnoremap <space>r :<C-u>QuickRun<CR>
nnoremap <space>br :<C-u>QuickRun -outputter browser<CR>

" tabnew by Space-t
nnoremap <space>t :<C-u>tabnew <C-d>

" Ctrl+ h/l to go/back to file
nnoremap <C-H> <C-O>
nnoremap <C-L> gf

" tabmove
nnoremap <C-Tab> :tabnext<CR>
nnoremap <C-S-Tab> :tabprevious<CR>

" buffer control by arrow keys
"nnoremap <DOWN>  :<C-u>bdelete<CR>
"nnoremap <UP>    :<C-u>ls<CR>
"nnoremap <RIGHT> :<C-u>bnext<CR>
"nnoremap <LEFT>  :<C-u>bprevious<CR>

" save and quit
nnoremap <space>w :<C-u>write<CR>
nnoremap <space>q :<C-u>quit<CR>

" insert a blank line by 1 stroke
nnoremap <C-o> o<ESC>k

autocmd FileType help nnoremap <buffer> q <C-w>q

au BufRead,BufNewFile *.applescript set filetype=applescript

" AutoCmd
" autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe
"normal! g`\"" | endif

" Map Leader (,) settings  {{{1
let mapleader=','
" insert date, time (from kana1)
inoremap <Leader>df <C-R>=strftime('%Y-%m-%dT%H:%M:%S+09:00')<CR>
inoremap <Leader>dd <C-R>=strftime('%Y%m%d')<CR>
cnoremap <Leader>dd <C-R>=strftime('%Y%m%d')<CR>
inoremap <Leader>dt <C-R>=strftime('%H:%M:%S')<CR>
" Copy/Paste via clipboard
vnoremap <Leader>cc "+y
vnoremap <Leader>cx "+y<Esc>gvd
nnoremap <Leader>cp "+p
" 1.2.3.4.5. expand numbers
nnoremap <Leader>ln <ESC>:s/\./\. \r/g<CR>
" for BrainPhantom
inoremap <Leader>bp <ESC>0la<CR><CR>(p.<ESC>A)<CR><<<ESC>kki
"}}}1

nnoremap <Leader>vl :source $MYVIMRC<CR>:source $HOME/.gvimrc<CR>
nnoremap <Leader>vs :tabnew $MYVIMRC<CR>

" pathogen-vim settings
runtime! autoload/pathogen.vim
if exists('g:loaded_pathogen')
    call pathogen#runtime_append_all_bundles()
    try
        call pathogen#helptags()
    catch /Duplicate tag/
        " noting
    endtry
end

" VimShell aliases
" TODO: add loaded_vimshell judgement
nnoremap <silent> ,is :VimShell<CR>
nnoremap <silent> ,irb :VimShellInteractive irb<CR>
nnoremap <silent> ,ss <S-v>:VimShellSendString<CR>
vmap <silent> ,ss :VimShellSendString<CR>

" fugitive.vim settings
nnoremap <Space>gd :<C-u>Gdiff<Enter>
nnoremap <Space>gs :<C-u>Gstatus<Enter>
nnoremap <Space>gl :<C-u>Glog<Enter>
nnoremap <Space>ga :<C-u>Gwrite<Enter>
nnoremap <Space>gc :<C-u>Gcommit<Enter>
nnoremap <Space>gC :<C-u>Git commit --amend<Enter>
nnoremap <Space>gb :<C-u>Gblame<Enter>

" unite.vim settings
let g:unite_enable_start_insert=1
nnoremap <silent> ,ub :<C-u>Unite buffer<CR>
nnoremap <silent> ,uf :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
nnoremap <silent> ,ur :<C-u>Unite -buffer-name=register register<CR>
nnoremap <silent> ,um :<C-u>Unite file_mru<CR>
nnoremap <silent> ,uu :<C-u>Unite buffer file_mru<CR>
nnoremap <silent> ,ua :<C-u>UniteWithBufferDir -buffer-name=files buffer file_mru bookmark file<CR>

au FileType unite nnoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
au FileType unite inoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
au FileType unite nnoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')
au FileType unite inoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')
au FileType unite nnoremap <silent> <buffer> <ESC><ESC> q
au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>q



" ------------------------- functions -------------------------
function! GetEFstatus() " {{{1
" GetEFstatus is a function which get file encording and fileformat, then abbreviate them.
" modified...original-> http://memo.officebrook.net/20050512.html
	let str = ''
	let fenc = ''
	if &fileformat == 'unix'
		"let str = '[U]'
		let str = 'unix'
	else
		"let str = '[' . &fileformat . ']'
		let str = &fileformat
	endif
	if &fileencoding != ''
		if &fileencoding =~# 'iso-2022-jp'
			let fenc = 'iso'
		elseif &fileencoding == 'utf-8'
			let fenc = 'utf'
		elseif &fileencoding == 'cp932'
			let fenc = 'S'
		elseif &fileencoding =~# 'euc-jp'
			let fenc = 'euc'
		else
			let fenc = &fileencoding
		endif
		"let str = str . '[' . fenc . ']'
		let str = str . '-' . fenc
	else
		let str = str . '[-]'
	endif
	unlet fenc
	return str
endfunction
"}}}1

"display end of line
set list
set listchars=tab:>\ ,trail:X,nbsp:%,extends:>,precedes:<
function! SOLSpaceHilight()
    syntax match SOLSpace "^\s\+" display containedin=ALL
    highlight SOLSpace term=underline ctermbg=LightRed
endf
function! JISX0208SpaceHilight()
    syntax match JISX0208Space "　" display containedin=ALL
    highlight JISX0208Space term=underline ctermbg=brown
endf
if has("syntax")
    syntax on
        augroup invisible
        autocmd! invisible
        autocmd BufNew,BufRead * call SOLSpaceHilight()
        autocmd BufNew,BufRead * call JISX0208SpaceHilight()
    augroup END
endif
