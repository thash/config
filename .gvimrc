"colorscheme murphy

if has('gui_macvim')

  cd ~/Desktop
  set showtabline=2
  set guifont=Osaka-Mono:h10
  " no scroll/tool bar
  set go=egmtc
  set transparency=20
  highlight LineNr ctermfg=darkgray

  colorscheme desert
  " No Full Screen size
  set lines=54
  set columns=90
  " Full Screen mode: MAX window size
  set fuoptions=maxvert,maxhorz
  ":macaction zoomAll:
  "map <silent> gw :macaction selectNextWindow:
  "map <silent> gW :macaction selectPreviousWindow:

elseif has("gui_win32")
" font setting from http://memo.xight.org/2007-11-01-2
    set guifont=meiryo:h16:cSHIFTJIS
    set printfont=gothic:h10:cSHIFTJIS
    " these fonts should be modified
    autocmd GUIEnter * winpos 200 100
    autocmd GUIEnter * winsize 150 50

endif
