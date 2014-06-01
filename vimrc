""
"" Janus setup
""

""set background=dark
set tabstop=2
set shiftwidth=2
set softtabstop=2
set smarttab
set expandtab

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

set number
syntax enable
set background=dark
"let g:solarized_termcolors=256
"let g:solarized_visibility= "high"
"let g:solarized_contrast = "high"
"colorscheme solarized

set expandtab
set shiftwidth=4
set softtabstop=4
set autoindent
let &t_Co=256
colors twilight256
