"https://github.com/brewster/brewster/compare/develop~5...develop""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Evan Carmi's .vimrc. http://ecarmi.org/
"   Credits: {{{1

"Use Vundle for managing plugings

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" My Bundles here:
"
" original repos on github

Bundle 'Lokaltog/vim-easymotion'
"-------------------------------
" change the default EasyMotion shading to something more readable with
" Solarized
hi link EasyMotionTarget ErrorMsg
hi link EasyMotionShade  Comment


Bundle 'altercation/vim-colors-solarized'
let g:solarized_termcolors=256

Bundle 'tpope/vim-rails.git'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'sjl/gundo.vim'
Bundle 'vim-scripts/ruby-matchit'
Bundle 'airblade/vim-gitgutter.git'

nnoremap <F5> :GundoToggle<CR>

Bundle 'https://github.com/Lokaltog/powerline'
" Powerline configuration
set laststatus=2   " Always show the statusline
set encoding=utf-8 " Necessary to show Unicode glyphs
let g:Powerline_symbols = 'fancy'

"Bundle 'nathanaelkane/vim-indent-guides'
"let g:indent_guides_guide_size=1


Bundle 'kana/vim-fakeclip'


Bundle 'kien/ctrlp.vim'

"Bundle "tpope/vim-markdown"
Bundle "puppetlabs/puppet-syntax-vim"

Bundle "scrooloose/nerdtree"
Bundle "scrooloose/syntastic"

Bundle 'thoughtbot/vim-rspec'
Bundle 'tpope/vim-dispatch'
" Rspec.vim mappings
map <Leader>r :call RunCurrentSpecFile()<CR>
map <Leader>rn :call RunNearestSpec()<CR>
map <Leader>rl :call RunLastSpec()<CR>
map <Leader>ra :call RunAllSpecs()<CR>
let g:rspec_command = "Dispatch rspec {spec}"

map <Leader>a :AgFromSearch<CR>

"----------------------
"Use system find command"
set wildignore+=*/tmp/*,*.so,*.swp,*.zip  " MacOSX/Linux

let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'
"let g:ctrlp_user_command = 'find %s -type f'

" Set the max files
let g:ctrlp_max_files = 10000

" Optimize file searching
if has("unix")
    let g:ctrlp_user_command = {
                \   'types': {
                \       1: ['.git/', 'cd %s && git ls-files']
                \   },
                \   'fallback': 'find %s -type f | head -' . g:ctrlp_max_files
                \ }
endif


" vim-scripts repos
Bundle 'L9'
Bundle 'FuzzyFinder'

filetype plugin indent on     " required!
"
" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..

"
" ====== General ===== {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sets how many lines of history VIM has to remember
set history=1000

syntax on


" Set to auto read when a file is changed from the outside
set autoread

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","
let g:mapleader = ","

" Wait 250 milliseconds before ignoring ambiguous mappings
" See :h timeout and :h map-ambiguous
set timeoutlen=250 "After 250 milliseconds run characters as a map

" Fast saving
nmap <leader>w :w<cr>

" Fast editing of the .vimrc
map <leader>e :e! ~/.vimrc<cr>

" Toggle NERDtree
map <leader>N :NERDTreeToggle<cr>

"Toggle taglist
let TList_Process_File_Always = 1
map <leader>u :TlistToggle<cr>
map <F4> :!/usr/bin/ctags-exuberant -R .<cr>

"Use jj as <Esc> so hands don't have to move
imap jj <Esc>

" When vimrc is edited, reload it
autocmd! bufwritepost .vimrc source ~/.vimrc
autocmd! BufWinEnter .vimrc set foldmethod=marker

"Close a window. Note: this doesn't close the buffer
map <leader>q :clo <cr>

" ====== Folds ===== {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" See General for foldmethod settings: need to be at top of file
" Don't close all folds by default when file loads
set nofoldenable

" Map <leader>f and <leader>F to open and close folds respectively
map <leader>f zO
map <leader>F zC

"Open a column to indicate open and closed folds
set foldcolumn=2

" ====== VIM user interface ====={{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set 3 lines to the cursors - when moving vertical there will always be this
" many lines above/below current cursor. Can be annoying if you use the mouse
" to click on top of the page, because it will move the page
set so=3
let &sbr = nr2char(8618).' '    " Show ↪ at the beginning of wrapped lines

set wildmenu "Turn on WiLd menu
set wildmode=longest,full "Even nicer tab-completion on the command line

set ruler "Always show current position

set cmdheight=1 "The commandbar height

set hid "Change buffer - without saving

" Set backspace config
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

set ignorecase "Ignore case when searching

set hlsearch "Highlight search things

set incsearch "Make search act like search in modern browsers

set magic "Set magic on, for regular expressions

set showmatch "Show matching bracets when text indicator is over them
set mat=2 "How many tenths of a second to blink

" No sound on errors
set noerrorbells
set novisualbell
set t_vb=

" ====== Colors and Fonts ====={{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax enable "Enable syntax hl

" Set font
"set gfn=Menlo 10
set shell=/bin/bash
set nu

set list listchars=tab:›\ ,trail:·,eol:¬ " mark trailing white space

if has("gui_running")
    "Remove menu bar(m), toolbar(T), right scroll bar(r), left scroll bar(l),
    "tabline
    set guioptions-=m
    set guioptions-=T
    set guioptions-=R
    set guioptions-=r
    set guioptions-=l
    set guioptions-=L
    set showtabline=0
    set background=light
else
  " Solarized stuff
  set background=dark
  " solarized options 
  let g:solarized_termcolors = 256
  let g:solarized_visibility = "high"
  let g:solarized_contrast = "high"
endif

colorscheme solarized

set encoding=utf8
try
    lang en_US
catch
endtry

set ffs=unix,dos,mac "Default file types

" ====== Files and backups ====={{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Leave backups on, but put in separate directory
"from http://stackoverflow.com/questions/1394350/vim-dont-save-backups-if-in-svn-working-copy
" Use a common directory for backups and swp files
" Create it if it doesn't exist
silent execute '!mkdir -p ~/.vim_backups'
set backupdir=~/.vim_backups//
set directory=~/.vim_backups//


" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set expandtab
set shiftwidth=2
set tabstop=2 "see help, other than 8 can mess up printing
set smarttab
set softtabstop=2 " allows backspace to delete 4 spaces
set vb "Visualbell, turn off audible bell

set lbr
"Set textwidth to 0; don't auto insert line breaks
set textwidth=80
set tw=0

set ai "Auto indent
set smartindent "Smart indet
set wrap "Wrap lines

"Handle python comment indentation - http://stackoverflow.com/questions/2360249/vim-automatically-removes-indentation-on-python-comments
inoremap # X#

map <leader>t2 :setlocal shiftwidth=2<cr>
map <leader>t4 :setlocal shiftwidth=4<cr>
map <leader>t8 :setlocal shiftwidth=4<cr>


" ====== Visual mode related ====={{{1
""""""""""""""""""""""""""""""
" Really useful!
"  In visual mode when you press * or # to search for the current selection
vnoremap <silent> * :call VisualSearch('f')<CR>
vnoremap <silent> # :call VisualSearch('b')<CR>

" When you press gv you vimgrep after the selected text
vnoremap <silent> gv :call VisualSearch('gv')<CR>
map <leader>g :vimgrep // **/*<left><left><left><left><left><left>

" From an idea by Michael Naumann
function! VisualSearch(direction) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gv'
        call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.')
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

" ====== Command mode related ====={{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Smart mappings on the command line
cno $h e ~/
cno $d e ~/Desktop/
cno $j e ./
cno $c e <C-\>eCurrentFileDir("e")<cr>

" $q is super useful when browsing on the command line
cno $q <C-\>eDeleteTillSlash()<cr>

" Bash like keys for the command line
cnoremap <C-A>          <Home>
cnoremap <C-E>          <End>
cnoremap <C-K>          <C-U>

cnoremap <C-P> <Up>
cnoremap <C-N> <Down>

" Useful on some European keyboards
map ½ $
imap ½ $
vmap ½ $
cmap ½ $


func! Cwd()
  let cwd = getcwd()
  return "e " . cwd 
endfunc

"Modify functions to only work on linux
func! DeleteTillSlash()
    let g:cmd = getcmdline()
    let g:cmd_edited = substitute(g:cmd, "\\(.*\[/\]\\).*", "\\1", "")

    if g:cmd == g:cmd_edited
        let g:cmd_edited = substitute(g:cmd, "\\(.*\[/\]\\).*/", "\\1", "")
    endif
    return g:cmd_edited
endfunc

func! CurrentFileDir(cmd)
    return a:cmd . " " . expand("%:p:h") . "/"
endfunc

" ====== Moving around, tabs and buffers ====={{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Map space to / (search) and c-space to ? (backgwards search)
map <space> /
map <c-space> ?
map <silent> <leader><cr> :noh<cr>

" Smart way to move btw. windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Close the current buffer
map <leader>bd :Bclose<cr>

" Close all the buffers
map <leader>ba :1,300 bd<cr>

" Use the arrows to something usefull
map <right> :bn<cr>
map <left> :bp<cr>

" Tab configuration
map <leader>tn :tabnew %<cr>
map <leader>te :tabedit 
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove 

" When pressing <leader>cd switch to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>


command! Bclose call <SID>BufcloseCloseIt()
function! <SID>BufcloseCloseIt()
   let l:currentBufNum = bufnr("%")
   let l:alternateBufNum = bufnr("#")

   if buflisted(l:alternateBufNum)
     buffer #
   else
     bnext
   endif

   if bufnr("%") == l:currentBufNum
     new
   endif

   if buflisted(l:currentBufNum)
     execute("bdelete ".l:currentBufNum)
   endif
endfunction

" Specify the behavior when switching between buffers 
try
  set switchbuf=usetab
catch
endtry


" ====== Statusline ====={{{1
""""""""""""""""""""""""""""""
" Always hide the statusline
set laststatus=2

" Format the statusline
set statusline=%<%f\ %h%w%m%r%y%=B:%o\ F:%{foldlevel('.')}\ L:%l/%L\ (%p%%)\ C:%c%V\ 

function! CurDir()
    let curdir = substitute(getcwd(), '/Users/amir/', "~/", "g")
    return curdir
endfunction


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Parenthesis/bracket expanding
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
vnoremap $1 <esc>`>a)<esc>`<i(<esc>
vnoremap $2 <esc>`>a]<esc>`<i[<esc>
vnoremap $3 <esc>`>a}<esc>`<i{<esc>
vnoremap $$ <esc>`>a"<esc>`<i"<esc>
vnoremap $q <esc>`>a'<esc>`<i'<esc>
vnoremap $e <esc>`>a"<esc>`<i"<esc>

" Map auto complete of (, ", ', [
inoremap $1 ()<esc>i
inoremap $2 []<esc>i
inoremap $3 {}<esc>i
inoremap $4 {<esc>o}<esc>O
inoremap $q ''<esc>i
inoremap $e ""<esc>i


" ====== General Abbrevs ====={{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
iab xdate <c-r>=strftime("%d/%m/%y %H:%M:%S")<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Editing mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Remap VIM 0
map 0 ^

"Move a line of text using ALT+[jk] or Comamnd+[jk] on mac
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

"Delete trailing white space, useful for Python ;)
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()


" ====== Cope ====={{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Do :help cope if you are unsure what cope is. It's super useful!
map <leader>cc :botright cope<cr>
map <leader>n :cn<cr>
map <leader>p :cp<cr>


" ====== bufExplorer plugin ====={{{1
""""""""""""""""""""""""""""""
let g:bufExplorerDefaultHelp=0
let g:bufExplorerShowRelativePath=1
let g:bufExplorerSortBy = "name"

" ====== Snipmate plugin ====={{{1
""""""""""""""""""""""""""""""

"Snipmates Django configs
autocmd FileType python set ft=python.django " For SnipMate
autocmd FileType html set ft=htmldjango.html " For SnipMate

" ====== Omni complete functions ====={{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType c set omnifunc=ccomplete#Complete


" ====== Spell checking ====={{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

"Shortcuts using <leader>
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=


" ====== Python section ====={{{1
""""""""""""""""""""""""""""""

set suffixes+=.pyc,.pyo "ignore compiled Python files

au FileType python set nocindent
au FileType python set tw=79 "Reminder of PEP8
let python_highlight_all = 1
au FileType python syn keyword pythonDecorator True None False self

au BufNewFile,BufRead *.jinja set syntax=htmljinja
au BufNewFile,BufRead *.mako set ft=mako

au FileType python inoremap <buffer> $r return 
au FileType python inoremap <buffer> $i import 
au FileType python inoremap <buffer> $p print 
au FileType python inoremap <buffer> $f #--- PH ----------------------------------------------<esc>FP2xi
au FileType python map <buffer> <leader>1 /class 
au FileType python map <buffer> <leader>2 /def 
au FileType python map <buffer> <leader>C ?class 
au FileType python map <buffer> <leader>D ?def 


" ====== Vim grep ====={{{1
""""""""""""""""""""""""""""""
let Grep_Skip_Dirs = 'RCS CVS SCCS .svn .git generated'
set grepprg=/bin/grep\ -nH


" ====== Miscellaneous ====={{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remove the Windows ^M - when the encodings gets messed up
"noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" ====== Additional Settings and Mappings ====={{{1
set showcmd "Show how much of a command you have typed so far.
set showmode "Show current mode
set nocompatible "Vi is crippled
set smartcase "Care about case in search if string contains uppercase letters

set fo=tcq "Formatting options, often used with comments and gq. Not sure if this is the best setting or not.

" prevents the display of vertical "@" characters when a line wraps past the"
" last line in a window (may not be necessary for windows xp)"
set display+=lastline

"highlight lines longer than 79 characters
nmap <leader>l :match ErrorMsg '\%>79v.\+'<cr>
"unhighlight lines
nmap <leader>L :match<cr>

"Toggle hlsearch
nmap <leader>h :set hls!<cr>

"Remember folds for foldmethod=manual. This is done by running :mkview whenever you close a buffer
"and :loadview when you open a new buffer.This only
"works on python and java and ruby files (which are more likely "to have folds)
au BufWinLeave *.java mkview
au BufWinEnter *.java silent loadview
"au BufWinLeave *.py mkview
"au BufWinEnter *.py silent loadview
au BufWinLeave *.rb mkview
au BufWinEnter *.rb silent loadview

" ====== Insert prepared datetime stamps ====={{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" :map <F9> insert a date-time stamp" - date-time stamp"
:map <F9> a<C-R>=strftime("<>%Y%m%d-%H%M%S - time check")<CR><Esc>

" Make F8 insert the date stamp" - weekday"
:map <F8> a<C-R>=strftime("<>%Y%m%d - %A")<CR><Esc>

" :map <F7> insert a date-time stamp" - weekday"
:map <F7> a<C-R>=strftime("%Y%m%d-%H%M%S - %A")<CR><Esc>

" Make the tab labels more useful. From http://blog.golden-ratio.net/2008/08/using-tabs-in-vim/
function! GuiTabLabel()
        " add the tab number
        let label = '['.tabpagenr()
 
        " modified since the last save?
        let buflist = tabpagebuflist(v:lnum)
        for bufnr in buflist
                if getbufvar(bufnr, '&modified')
                        let label .= '*'
                        break
                endif
        endfor
 
        " count number of open windows in the tab
        let wincount = tabpagewinnr(v:lnum, '$')
        if wincount > 1
                let label .= ', '.wincount
        endif
        let label .= '] '
 
        " add the file name without path information
        let n = bufname(buflist[tabpagewinnr(v:lnum) - 1])
        let label .= fnamemodify(n, ':t')
 
        return label
endfunction
 
set guitablabel=%{GuiTabLabel()}


iabbrev rdb    require 'ruby-debug'; Debugger.start; Debugger.settings[:autoeval] = 1; Debugger.settings[:autolist] = 1; debugger
iabbrev pry    binding.pry
iabbrev rdebug    require 'ruby-debug'; Debugger.start; Debugger.settings[:autoeval] = 1; Debugger.settings[:autolist] = 1; debugger
iabbrev pdb    import pdb; pdb.set_trace()

""" RUBY"""
"Check syntax on file save
autocmd FileType ruby map <leader>c :w<CR>:!ruby -c %<CR>

autocmd BufWinEnter *.rabl set ft=ruby

" CtrlP
"Add keymappings
map <leader>t :CtrlP<cr>
map <leader>b :CtrlPBuffer<cr>

set enc=utf-8

set path +=


" Linux Ubuntu settings
"if has("unix")
"  if has("autocmd")
"    au InsertEnter * silent execute "!gconftool-2 --type string --set /apps/gnome-terminal/profiles/Default/cursor_shape ibeam"
"    au InsertLeave * silent execute "!gconftool-2 --type string --set /apps/gnome-terminal/profiles/Default/cursor_shape block"
"    au VimLeave * silent execute "!gconftool-2 --type string --set /apps/gnome-terminal/profiles/Default/cursor_shape ibeam"
"  endif
"endif

"Highlight current line in insert
autocmd InsertEnter,InsertLeave * set cul!

"Set .md as markdown, not coding much in Modula2 these days.
autocmd BufNewFile,BufRead *.md set filetype=markdown

" LaTex
autocmd BufNewFile,BufRead *.tex set makeprg=pdflatex\ -shell-escape\ %

"Auto save buffer on focus lost
autocmd FocusLost * :w

map <leader>c <esc>0"+y$

map <leader>, :make<cr><cr><cr>
cmap w!! w !sudo tee >/dev/null %

set clipboard=unnamed
Bundle 'rking/ag.vim'

" Enable syntax highlighting when buffers were loaded through :bufdo, which
" disables the Syntax autocmd event to speed up processing.
augroup EnableSyntaxHighlighting
    " Filetype processing does happen, so we can detect a buffer initially
    " loaded during :bufdo through a set filetype, but missing b:current_syntax.
    " Also don't do this when the user explicitly turned off syntax highlighting
    " via :syntax off.
    " Note: Must allow nesting of autocmds so that the :syntax enable triggers
    " the ColorScheme event. Otherwise, some highlighting groups may not be
    " restored properly.
    autocmd! BufWinEnter * nested if exists('syntax_on') && ! exists('b:current_syntax') && ! empty(&l:filetype) | syntax enable | endif

    " The above does not handle reloading via :bufdo edit!, because the
    " b:current_syntax variable is not cleared by that. During the :bufdo,
    " 'eventignore' contains "Syntax", so this can be used to detect this
    " situation when the file is re-read into the buffer. Due to the
    " 'eventignore', an immediate :syntax enable is ignored, but by clearing
    " b:current_syntax, the above handler will do this when the reloaded buffer
    " is displayed in a window again.
    autocmd! BufRead * if exists('syntax_on') && exists('b:current_syntax') && ! empty(&l:filetype) && index(split(&eventignore, ','), 'Syntax') != -1 | unlet! b:current_syntax | endif
augroup END

set ttyfast
set lazyredraw
