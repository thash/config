set tabstop=4
set shiftwidth=4
set expandtab
set nocompatible
set ignorecase
set autoindent
set noswapfile
set incsearch
set nowrapscan
set number
set cursorline
set foldmethod=marker
set visualbell
set laststatus=2
set statusline=%F%r%m%h%w%=%l/%L(%3p%%)\ FileType:%y/Form:%{GetEFstatus()}
"set backupdir=$VIM/tmp
"set dictionary=$VIM/dict/java14.dict

syntax on
filetype on
filetype indent on 
filetype plugin on

" Color Settings
" execute ':so $VIMRUNTIME/syntax/colortest.vim' to view sample colors
colorscheme desert
highlight LineNr ctermfg=darkgray
highlight Visual ctermfg=darkblue ctermbg=grey
highlight VisualNOS ctermfg=darkblue ctermbg=grey
highlight StatusLine cterm=reverse,bold
highlight StatusLine ctermfg=green ctermbg=white
highlight StatusLineNC cterm=reverse
" change statusline color in insert mode
autocmd InsertEnter * highlight StatusLine ctermfg=red
autocmd InsertLeave * highlight StatusLine ctermfg=green


" 文字コードの自動認識 {{{1
if &encoding !=# 'utf-8'
  set encoding=japan
  set fileencoding=japan
endif
if has('iconv')
  let s:enc_euc = 'euc-jp'
  let s:enc_jis = 'iso-2022-jp'
  " iconvがeucJP-msに対応しているかをチェック
  if iconv("\x87\x64\x87\x6a", 'cp932', 'eucjp-ms') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'eucjp-ms'
    let s:enc_jis = 'iso-2022-jp-3'
  " iconvがJISX0213に対応しているかをチェック
  elseif iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
    let s:enc_euc = 'euc-jisx0213'
    let s:enc_jis = 'iso-2022-jp-3'
  endif
  " fileencodingsを構築
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
  " 定数を処分
  unlet s:enc_euc
  unlet s:enc_jis
endif
" 日本語を含まない場合は fileencoding に encoding を使うようにする
if has('autocmd')
  function! AU_ReCheck_FENC()
    if &fileencoding =~# 'iso-2022-jp' && search("[^\x01-\x7e]", 'n') == 0
      let &fileencoding=&encoding
    endif
  endfunction
  autocmd BufReadPost * call AU_ReCheck_FENC()
endif
" 改行コードの自動認識
set fileformats=unix,dos,mac
" □とか○の文字があってもカーソル位置がずれないようにする
if exists('&ambiwidth')
  set ambiwidth=double
endif

" フォント設定 from http://memo.xight.org/2007-11-01-2
if has("gui_win32")
    " set guifont=ＭＳ_ゴシック:h9:cSHIFTJIS
    set guifont=meiryo:h16:cSHIFTJIS
    " set guifont=メイリオ:h9:cSHIFTJIS
    set printfont=ＭＳ_ゴシック:h10:cSHIFTJIS
    " set printfont=メイリオ:h10:cSHIFTJIS
    autocmd GUIEnter * winpos 200 100
    autocmd GUIEnter * winsize 100 50
endif

"}}}1


" Replace colon with semi-colon
nnoremap ; :
vnoremap ; :
" use colon as a single quote
nnoremap : '

" 移動は常に表示ラインでの移動。
noremap j gj
noremap k gk
noremap 0 g0
noremap $ g$
noremap gj j
noremap gk k
noremap g0 0
noremap g$ $
" hjkl move in insert mode
"inoremap <C-j> <DOWN>
"inoremap <C-k> <UP>
"inoremap <C-l> <RIGHT>
"inoremap <C-h> <LEFT>

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


" window間の移動、拡大縮小
nnoremap <space>j <C-W>j
nnoremap <space>k <C-W>k
nnoremap <space>h <C-W>h
nnoremap <space>l <C-W>l
nnoremap <space>+ <C-W>5+
nnoremap <space>- <C-W>5-
nnoremap <space>> <C-W>10>
nnoremap <space>< <C-W>10<
"Ruby
nnoremap <space>r :<C-u>!ruby %<CR>

" 矢印キーでバッファ操作
"nnoremap <DOWN>  :<C-u>bdelete<CR>
nnoremap <UP>    :<C-u>ls<CR>
nnoremap <RIGHT> :<C-u>bnext<CR>
nnoremap <LEFT>  :<C-u>bprevious<CR>

" ujihisa流、保存と終了
nnoremap <space>w :<C-u>write<CR>
nnoremap <space>q :<C-u>quit<CR>

" 空白行を追加する操作を１ストロークでやりたい
nnoremap <C-o> o<ESC>k

autocmd FileType help nnoremap <buffer> q <C-w>q

" 以前開いていたときのカーソル位置を復元する
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
" 1.2.3.4.5.を箇条書き
nnoremap <Leader>ln <ESC>:s/\./\. \r/g<CR>
" for BrainPhantom
inoremap <Leader>bp <ESC>0la<CR><CR>(p.<ESC>A)<CR><<<CR><CR>-<ESC>kkkki
"}}}1

nnoremap <Leader>vl :source $MYVIMRC<CR>
nnoremap <Leader>vs :vs $MYVIMRC<CR>




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

" ------------------------- disabled ------------------------- 
"disabled {{{1

"vim-latex {{{2
" REQUIRED. This makes vim invoke latex-suite when you open a tex file.
filetype plugin on

" IMPORTANT: win32 users will need to have 'shellslash' set so that latex
" can be called correctly.
" \ -> /
" set shellslash

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse latex-suite. Set your grep
" program to alway generate a file-name.
set grepprg=grep\ -nH\ $*
" OPTIONAL: This enables automatic indentation as you type. 
" }}}2
" Alignを日本語環境で使用するための設定 {{{2
" let g:Align_xstrlen = 3
" 実際には、この設定は完璧には機能していないようなので、
" 現段階では設定を追加しなくても良いと思います。 -> コメントアウト
" }}}2

" 文字数カウント
" vnoremap <F1> :s/./&/g<CR>

" Mapping for Tab
" C-tの避難:戻る挙動はC-[に割り当てる(↑)
" tagstack errorを引き起こすため止める。
"nnoremap <C-t> :tabedit
"nnoremap <C-Tab> :tabNext<CR>
"nnoremap <C-S-Tab> :tabPrevious<CR>
"nnoremap <C-[> <C-t>

" Highlight ZENKAKU Space and end of line 
"function! s:HighlightSpaces()
"	syntax match WideSpace /　/ containedin=ALL
"	syntax match EOLSpace /\s\+$/ containedin=ALL
"endf
"call s:HighlightSpaces()
"autocmd WinEnter * call s:HighlightSpaces()
"highlight WideSpace ctermbg=blue guibg=blue
"highlight EOLSpace ctermbg=red guibg=red


" }}}1

" autoconplete+ from id:Ubuntuさん {{{1
" Vimの自動補完を大文字と数字にも対応させる - Hatena::Diary::Ubuntu 
" <http://d.hatena.ne.jp/Ubuntu/20080124/1201139267>
"set completeopt=menuone,preview
"
"function! CompleteWithoutInsert()
"	return "\<C-n>\<C-r>=pumvisible() ? \"\\<C-P>\\<C-N>\\<C-P>\": \"\"\<CR>"
"endfunction
"
"inoremap <expr> <C-n> pumvisible() ? "\<C-n>" : CompleteWithoutInsert()
"
"let letter = "a"
"while letter <=# "z"
"	execute 'inoremap <expr> ' letter ' "' . letter . '" . (pumvisible() ? "" : CompleteWithoutInsert())'
"	let letter = nr2char(char2nr(letter) + 1)
"endwhile
"let letter = "A"
"while letter <=# "Z"
"	execute 'inoremap <expr> ' letter ' "' . letter . '" . (pumvisible() ? "" : CompleteWithoutInsert())'
"	let letter = nr2char(char2nr(letter) + 1)
"endwhile
"let letter = "0"
"while letter <=# "9"
"	execute 'inoremap <expr> ' letter ' "' . letter . '" . (pumvisible() ? "" : CompleteWithoutInsert())'
"	let letter = nr2char(char2nr(letter) + 1)
"endwhile
"
"inoremap <expr> <CR> pumvisible() ? "\<C-Y>\<CR>" : "\<CR>"
"set lazyredraw
" }}}1
