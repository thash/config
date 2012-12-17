" Plugins with Gvim
Bundle 'colorsel.vim'

if has('gui_macvim')

  CdCurrent
  set cursorcolumn
  set showtabline=2
  "set guifont=Ricty:12
  set guifont=Monaco
  " no scroll/tool bar
  set go=egmtc
  set transparency=20
  highlight LineNr ctermfg=darkgray
  highlight Visual guibg=yellow guifg=darkgray

  colorscheme torte
  " No Full Screen size
  set lines=54
  set columns=150
  " Full Screen mode: MAX window size
  set fuoptions=maxvert,maxhorz
  ":macaction zoomAll:

  set visualbell t_vb=

  let g:returnApp = "MacVim"

  " C-Space to ESC, work only on gvim
  inoremap <C-Space> <C-[>
  cnoremap <C-Space> <C-[>
  nnoremap <C-Space> <C-[>

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
  set background=dark
  colorscheme solarized
  set novisualbell
  set guifont=Ricty

  set lines=50

  highlight LineNr ctermfg=darkgray
  highlight Visual guibg=yellow guifg=darkgray

  " C-Space to ESC, work only on gvim
  inoremap <C-Space> <C-[>
  cnoremap <C-Space> <C-[>
  nnoremap <C-Space> <C-[>

  " Linux have no Command key.
  nnoremap <Leader>pp "+gP
  vnoremap <C-X> "+x
  vnoremap <C-C> "+y
endif
