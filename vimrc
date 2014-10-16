set wildmenu


"mapping to save and gulp build
nmap <Leader>r :w !gulp build 
nmap <Leader>c :w !gulp css 
nmap <Leader>j :w !gulp js 

"open nerdtree at the right time
function! StartUp()
    if 0 == argc()
        NERDTree
    end
endfunction

autocmd VimEnter * call StartUp()

"jquery highlighting
au BufRead,BufNewFile jquery.*.js set ft=javascript syntax=jquery



""
"" Janus setup
""
execute pathogen#infect('~/.vim/bundle/{}')
"let g:Powerline_symbols = "fancy"
set laststatus=2


set mouse=a
set t_Co=256
" Define paths
"let g:janus_path = escape(fnamemodify(resolve(expand("<sfile>:p")), ":h"), ' ')
"let g:janus_vim_path = escape(fnamemodify(resolve(expand("<sfile>:p" . "vim")), ":h"), ' ')
"let g:janus_custom_path = expand("~/.janus")

" Source janus's core
"exe 'source ' . g:janus_vim_path . '/core/before/plugin/janus.vim'

" You should note that groups will be processed by Pathogen in reverse
" order they were added.
"call janus#add_group("tools")
"call janus#add_group("langs")
"call janus#add_group("colors")

""
"" Customisations
""

"if filereadable(expand("~/.vimrc.before"))
"  source ~/.vimrc.before
"endif


" Disable plugins prior to loading pathogen
"exe 'source ' . g:janus_vim_path . '/core/plugins.vim'

""
"" Pathogen setup
""

" Load all groups, custom dir, and janus core
"call janus#load_pathogen()

" .vimrc.after is loaded after the plugins have loaded

"execute pathogen#infect()

"tagbar
nmap <F8> :TagbarToggle<CR>

"ctrl-p
set runtimepath^=~/.vim/bundle/ctrlp.vim
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_match_window_bottom = 0

"NERDTree
hi Directory guifg=#FF0000 ctermfg=117

"Remove | pipe symbols from diviers
set fillchars+=vert:\ 

set number
syntax enable
"let g:solarized_termcolors=256
"let g:solarized_visibility= "high"
"let g:solarized_contrast = "high"
"colorscheme solarized
hi TabLine      guifg=#ff5f00 guibg=#0000d7 gui=none ctermfg=202 ctermbg=20 cterm=none
hi TabLineSel   guifg=#666 guibg=#222 gui=bold ctermfg=202 ctermbg=20 cterm=bold
hi TabLineFill  guifg=#999 guibg=#222 gui=none ctermfg=202 ctermbg=20 cterm=none


set expandtab
set shiftwidth=2
set softtabstop=2
"set autoindent
let &t_Co=256

set background=dark
colors mirodark
"colors twilight256
