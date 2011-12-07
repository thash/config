" Vundle ============================================ {{{1
" Vundle initialization {{{2

set nocompatible
filetype off "here off, and after vundle finish, on again.
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

nmap <C-l> <Plug>(openbrowser-open)
Bundle 'gmarik/vundle'

" Vundle Define active plugins {{{2

Bundle 'fugitive.vim'
Bundle 'surround.vim'
Bundle 'ruby.vim'
Bundle 'taq/vim-rspec'
Bundle 'tpope/vim-rails'
Bundle 'Align'
Bundle 'project.tar.gz'
Bundle 'ruby-matchit'
Bundle 'catn.vim'
Bundle 'quickrun.vim'
Bundle 'open-browser.vim'
Bundle 'Markdown'
Bundle 'snipMate'
Bundle 'TwitVim'
Bundle 'ref.vim'
Bundle 'browsereload-mac.vim'
Bundle 'kchmck/vim-coffee-script'
Bundle 'shadow.vim'
Bundle 'mattn/webapi-vim'
Bundle 'textobj-user'
Bundle 'Gist.vim'
Bundle 'zef/vim-cycle'
Bundle 'The-NERD-Commenter'
"Bundle 'buftabs'

" Unite, and new generation plugins
Bundle 'unite.vim'
Bundle 'h1mesuke/unite-outline'
Bundle 'basyura/unite-rails'
Bundle 'Sixeight/unite-grep'
" Bundle 'neocomplcache'
" Bundle 'Shougo/vimshell'
" " installed by git clone...
" Bundle 'Shougo/vimproc'
" Bundle 'proc.vim'
" Bundle 'vimfiler'

" Bundle 'Shougo/vimshell'
" ColorSchemes, Syntax
Bundle 'molokai'
Bundle 'railscasts'
Bundle 'HTML5-Syntax-File'


" General ============================================ {{{1
" set someting {{{2
set encoding=utf8
set fileencoding=utf8
set autoindent
set expandtab
set foldmethod=marker
set foldlevel=2
set tags=./tags
set incsearch
set ignorecase
set laststatus=2
set number
set statusline=%F%r%m%h%w%=%l/%L(%3p%%)\ FileType:%y/Form:%{GetEFstatus()}
set shiftwidth=2
set tabstop=2
set noswapfile
set nowrapscan
set helplang=ja,en
set splitbelow
set splitright
set visualbell t_vb=
set cmdwinheight=12
set gdefault " all substitution
set backupdir=~/tmp,$VIM/tmp

" cursorline settings {{{2
set cursorline

  augroup cch
    autocmd! cch
    autocmd WinLeave * set nocursorline
    autocmd WinEnter,BufRead * set cursorline
  augroup END

:hi clear CursorLine
:hi CursorLine gui=underline
highlight CursorLine ctermbg=black guibg=black

" filetype settings + additional {{{2
filetype plugin indent on
syntax on
" skeleton loading autocmd {{{3
augroup SkeletonLoad
    autocmd! SkeletonLoad
    autocmd BufNewFile *.html 0r $HOME/.vim/templates/skel.html
augroup END

" filetype autocmd {{{3
augroup MyAutoCmdFileType
    autocmd! MyAutoCmdFileType
    autocmd FileType ruby setl autoindent
    " smart indent is disabled when paste is on
    autocmd FileType ruby setl nopaste
    autocmd FileType ruby setl smartindent cinwords=if,elsif,else,for,begin,def,class
    autocmd FileType ruby.rspec setl smartindent cinwords=describe,it,expect
    autocmd FileType help nnoremap <buffer> q <C-w>q
    autocmd BufRead,BufNewFile *.applescript set filetype=applescript
    autocmd BufRead,BufWinEnter,BufNewFile *.erb set filetype=eruby.html
    autocmd BufRead,BufNewFile *.md :call ChangeDir()
    autocmd Filetype xml inoremap <buffer> </ </<C-x><C-o>
    autocmd Filetype html inoremap <buffer> </ </<C-x><C-o>
    autocmd Filetype xhtml inoremap <buffer> </ </<C-x><C-o>
augroup END

" omni completion setting {{{3
autocmd FileType *
\   if &l:omnifunc == ''
\ |   setlocal omnifunc=syntaxcomplete#Complete
\ | endif
" autocmd definitions {{{2
augroup MyAutoCmdGeneral
    autocmd! MyAutoCmdGeneral
    " automatically move to directory file exist
    " autocmd BufEnter * call ChangeDir()

    " automatically move to last line
    autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

    " automatically remove spaces end of the line
    autocmd BufWritePre * :%s/\s\+$//ge

    " automatically add timestamp to backup files
    autocmd BufWritePre * let &bex = '-' . strftime('%Y%m%d_%Hh') . '~'

augroup END

" New undo-persistence feature of vim73 {{{2
" FYI: how to check Vim version, extentions
" http://vim-users.jp/2010/01/hack115/
if has('persistent_undo')
    set undofile
    set undodir=./.undofiles,$VIM/.undofiles
endif

" Local .vimrc {{{2
if filereadable(expand('~/.vimrc.local'))
    source ~/.vimrc.local
endif

" Color/Layout Settings ============================================ {{{1
" colorscheme {{{2
colorscheme  desert "molokai, murphy, darkblue

" detailed color {{{2
" FYI: execute ':so $VIMRUNTIME/syntax/colortest.vim' to view sample colors
highlight LineNr ctermfg=darkgray
highlight Visual ctermfg=darkblue ctermbg=grey
highlight VisualNOS ctermfg=darkblue ctermbg=grey
highlight StatusLine cterm=reverse,bold
highlight StatusLine ctermfg=green ctermbg=white
highlight StatusLineNC cterm=reverse
" Fold-colors
highlight Folded ctermbg=gray ctermfg=darkred guibg=grey guifg=blue
highlight FoldColumn guibg=darkgrey guifg=white ctermbg=darkgrey ctermfg=white
" change statusline color in insert mode
autocmd InsertEnter * highlight StatusLine ctermfg=red
autocmd InsertLeave * highlight StatusLine ctermfg=green
" display listchars (spaces at end of line, tab etc) {{{2
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



" Key remappings ============================================ {{{1
" general keys {{{2
inoremap <Nul> <C-[>
cnoremap <Nul> <C-[>
nnoremap <Nul> <C-[>
nnoremap <F1> <C-[>
inoremap <F1> <C-[>
" always reset iminsert to zero when leaving Insert mode.
inoremap <silent> <ESC> <ESC>:set iminsert=0<CR>

" Replace colon with semi-colon
nnoremap ; :
vnoremap ; :

" move by one display line
noremap j gj
noremap k gk
noremap 0 g0
noremap $ g$
noremap gj j
noremap gk k
noremap g0 0
noremap g$ $

" don't include newline in visual mode
vnoremap $ $h

noremap gJ J
noremap J gJ

nnoremap <C-a> 0
nnoremap <C-e> $

inoremap <C-e> <ESC>$a
inoremap <C-a> <ESC>0i

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
nnoremap <silent> <Esc><Esc> :nohlsearch<CR><Esc>

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

" tabnew, tabmove
nnoremap <space>t :<C-u>tabnew <C-d>
nnoremap <C-Tab> :tabnext<CR>
nnoremap <C-S-Tab> :tabprevious<CR>

" Open/Close a fold.
nnoremap fo za
nnoremap FO zR
nnoremap fO zM

" Open help
nnoremap <C-Y> :<C-u>h<space>

" Ctrl+ h/l to go/back to file
nnoremap <C-H> <C-O>
nnoremap <C-L> gf

" tags
nnoremap tl  <C-]>
vnoremap tl  <C-]>
nnoremap tt  :<C-u>tag<CR>
nnoremap th  :<C-u>pop<CR>
nnoremap tj  :<C-u>tags<CR>

" select last modified line (gm has a original meaning)
nnoremap gm '.V
" select last pasted block
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

" mark, jump
nnoremap <space>ma :<C-u>marks<CR>
nnoremap <space>ms m'
nnoremap <space>mj ''zz

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

" Map Leader (,) settings  {{{2
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
" vimrc better handling
nnoremap <Leader>vl :source $MYVIMRC<CR>:source $HOME/.gvimrc<CR>
nnoremap <Leader>vs :vs $MYVIMRC<CR>

" grep and get titles (markdown)
nnoremap <Leader>gp :<C-u>vimgrep /^#/ %<CR>:cwin<CR>


" exec Whoami() to show file name
nnoremap <Leader>fn :<C-u>call Whoami()<CR>

" toggle nonumber (used often for copy&paste)
nnoremap <Leader>nn :<C-u>set number!<CR>

" plugin settings ============================================ {{{1
" QuickRun settings {{{2
" execute current window using QuickRun
augroup MyAutoCmdRSpec
    autocmd! MyAutoCmdRSpec
    autocmd BufWinEnter,BufNewFile *_spec.rb set filetype=ruby.rspec
augroup END
let g:quickrun_config = {}
if executable('bundle exec rspec')
    let g:quickrun_config['ruby.rspec'] = {'command': 'bundle exec rspec'}
else
    let g:quickrun_config['ruby.rspec'] = {'command': 'rspec'}
endif
nnoremap <space>r :<C-u>QuickRun<CR>
nnoremap <space>ro :<C-u>QuickRun -outputter browser<CR>


" unite.vim settings {{{2
let g:unite_enable_start_insert=0
let g:unite_split_rule="topleft"
let g:unite_update_time=10
let g:unite_cursor_line_highlight="CursorLine"
let g:unite_source_file_ignore_pattern='vendor/bundle'
nnoremap <silent> ,ub :<C-u>Unite buffer -auto-preview<CR>
nnoremap <silent> ,uf :<C-u>UniteWithBufferDir -buffer-name=files file -auto-preview<CR>
nnoremap <silent> ,ur :<C-u>Unite -buffer-name=register register -auto-preview<CR>
nnoremap <silent> ,uu :<C-u>UniteWithCurrentDir -buffer-name=files file buffer -auto-preview<CR>
nnoremap <silent> ,uo :<C-u>Unite outline -auto-preview<CR>
nnoremap <silent> ,um :<C-u>Unite mapping -auto-preview<CR>
"nnoremap <silent> ,ur :<C-u>Unite rails/<C-d>
nnoremap <silent> ,ua :<C-u>UniteWithBufferDir -buffer-name=files buffer file_mru bookmark file -auto-preview<CR>

autocmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()
  nmap <buffer> <C-j> <Plug>(unite_select_next_line)
  imap <buffer> <C-j> <Plug>(unite_select_next_line)
  nmap <buffer> <C-k> <Plug>(unite_select_previous_line)
  imap <buffer> <C-k> <Plug>(unite_select_previous_line)
  nmap <silent> <buffer> <expr> <C-s> unite#do_action('split')
  imap <silent> <buffer> <expr> <C-s> unite#do_action('split')
  nmap <silent> <buffer> <expr> <C-v> unite#do_action('vsplit')
  imap <silent> <buffer> <expr> <C-v> unite#do_action('vsplit')
  nmap <silent> <buffer> <ESC><ESC> q
  imap <silent> <buffer> <ESC><ESC> <ESC>q
endfunction

" VimShell settings and aliases {{{2

if has('mac')
  let g:vimproc_dll_path = "/Users/hash/.vim/bundle/vimproc/autoload/proc.so"
endif

" TODO: add loaded_vimshell judgement
nnoremap <silent> ,is :VimShell<CR>
nnoremap <silent> ,irb :VimShellInteractive irb<CR>
nnoremap <silent> ,ss <S-v>:VimShellSendString<CR>
vmap <silent> ,ss :VimShellSendString<CR>

" fugitive.vim settings {{{2
nnoremap <Space>gd :<C-u>Gdiff<Enter>
nnoremap <Space>gs :<C-u>Gstatus<Enter>
nnoremap <Space>gl :<C-u>Glog<Enter>
nnoremap <Space>ga :<C-u>Gwrite<Enter>
nnoremap <Space>gc :<C-u>Gcommit<Enter>
nnoremap <Space>gC :<C-u>Git commit --amend<Enter>
nnoremap <Space>gb :<C-u>Gblame<Enter>

" Rails settings {{{2
let g:rubycomplete_rails = 1

" Openbrowser settings {{{2
let g:netrw_nogx = 1 " disable netrw's gx mapping.
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)

" buftabs settings {{{2
let g:buftabs_in_statusline=1
let g:buftabs_only_basename=1

" vim-cycle settings {{{2
let g:cycle_no_mappings=1
" call AddCycleGroup(['-', '_'])
nmap <C-C> <Plug>CycleNext

" Functions my/someone's ============================================ {{{1
" Automatic recognition of Encoding {{{2
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
"}}}2
function! GetEFstatus() " {{{2
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

" :TabpageCD {{{2
" from http://kana.github.com/config/vim/
" command! -nargs=? TabpageCD
"             \   execute 'cd' fnameescape(<q-args>)
"             \ | let t:cwd = getcwd()
"
" autocmd MyAutoCmd TabEnter *
"             \   if !exists('t:cwd')
"             \ |   let t:cwd = getcwd()
"             \ | endif
"         \ | execute 'cd' fnameescape(t:cwd)
"
" AlterCommand cd  TabpageCD



" move to current (opened file exists in) dir {{{2
function! ChangeDir()
    let _dir = expand("%:p:h")
    exec "cd " . _dir
    unlet _dir
endfunction

" show current file name {{{2
function! Whoami()
    exec 'echo expand("%")'
endfunction

" search word and open result in splitt window {{{2
command! -nargs=* Nameru call Nameru(<f-args>)
function! Nameru(word)
  exec 'vimgrep /' . a:word . '/ % | cwin'
endfunction

