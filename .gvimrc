if has('gui_macvim')

  " Prefer English menu
  source $VIMRUNTIME/delmenu.vim
  set langmenu=none
  source $VIMRUNTIME/menu.vim

  set guioptions=

  if has("multi_lang")
    language C
  endif

  "" enabling cursorcolumn slow down cursor
  set nocursorcolumn
  set showtabline=2
  set guifont=Ricty\ Diminished:h10
  set guifontwide=Ricty\ Diminished:h10
  " set guifont=Monaco
  " no scroll/tool bar
  set go=egmtc
  set transparency=25
  " highlight LineNr ctermfg=darkgray
  " highlight Visual guibg=yellow guifg=darkgray

  " set spell
  " set spelllang+=cjk
  " set spellfile=~/.vim/spell/en.utf-8.add

  " colorscheme pencil
  set background=dark

  " No Full Screen size
  set lines=80
  set columns=160
  " Full Screen mode: MAX window size
  set fuoptions=maxvert,maxhorz
  ":macaction zoomAll:

  set visualbell t_vb=

  let g:returnApp = "MacVim"

  augroup indentGuidesGUI
    autocmd! indentGuidesGUI
    autocmd WinEnter,BufRead * highlight IndentGuidesOdd  guibg=darkgray
    autocmd WinEnter,BufRead * highlight IndentGuidesEven guibg=black
  augroup END

elseif has("gui_win32")
" font setting from http://memo.xight.org/2007-11-01-2
    set guifont=meiryo:h16:cSHIFTJIS
    set printfont=gothic:h10:cSHIFTJIS
    " these fonts should be modified
    autocmd GUIEnter * winpos 200 100
    autocmd GUIEnter * winsize 150 50

elseif has('gui')

  set guioptions=
  " set background=dark
  " colorscheme solarized
  colorscheme gentooish
  set novisualbell
  set guifont=Ricty

  set lines=50

  highlight LineNr ctermfg=darkgray
  highlight Visual guibg=yellow guifg=darkgray

  " Linux have no Command key.
  nnoremap <Leader>pp "+gP
  vnoremap <C-X> "+x
  vnoremap <C-C> "+y
endif
