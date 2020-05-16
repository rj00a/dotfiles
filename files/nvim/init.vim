" TODO: Explore airline customizations.
" TODO: Termdebug function key mappings.

" Plugin management with vim-plug.
call plug#begin('~/.local/share/nvim/plugged')
Plug 'cespare/vim-toml'
Plug 'dag/vim-fish'
Plug 'google/vim-searchindex'
Plug 'junegunn/fzf.vim'
Plug 'kovetskiy/sxhkd-vim'
Plug 'machakann/vim-highlightedyank'
Plug 'moll/vim-bbye'
Plug 'rust-lang/rust.vim'
Plug 'tmhedberg/matchit'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vmchale/ats-vim'
Plug 'ziglang/zig.vim'
call plug#end()

" Enable line numbers.
set number

" Line numbers are close to the edge of the window.
set numberwidth=2

" Disable visual line wrap.
"set nowrap

" Set column limit for formatting with 'gq'.
set textwidth=100

" Do not move the cursor to the first column when typing '#' in .c files.
set cinkeys=0{,0},0),:,!^F,o,O,e

" Incremental Search.
" Jumps to the next match as you type.
set incsearch

" Highlight search results.
set hlsearch

" Disable case sensitive searches.
set ignorecase

" Become case sensitive while searching only if search string contains at least one capital letter.
set smartcase

" Vertical splits will put new windows right of the current window.
" Horizontal splits will put new windows above the current window.
set splitright

" Enable mouse support.
set mouse+=a

" In Gvim, remove the menu bar, toolbar, and scrollbar completely.
set guioptions=aegit

" Hide the tab line.
set showtabline=0

" Treat octal numbers like decimal numbers, becuase that's what they are most of the time.
set nrformats=bin,hex

" Don't insert two spaces after sentence-ending punctuation with a join command.
set nojoinspaces

" Hide the startup message.
set shortmess=I

" Share with system clipboard.
set clipboard^=unnamedplus

" Automatically reload files when they are modified externally.
set autoread

" checktime makes autoread actually do what it's supposed to do.
au CursorHold * checktime

" Make history greater than default.
set history=500

" Always show the stuff at the bottom.
set ruler

" Less backslashes needed in regular expressions by default.
set magic

" Disable annoying error sounds.
set noerrorbells
set novisualbell

" Don't redraw screen while executing macro.
" Good for performance.
set lazyredraw

" Disable swap files, because we have version control anyway.
set noswapfile

" Shows a vertial completion menu when pressing <tab> in the command buffer.
set wildmenu
set wildmode=longest:full,full

" Shows the input of an incomplete command.
set showcmd

" Allows switching between multiple unsaved buffers.
set hidden

" Backspace unconditionally works against auto indentation and more.
set backspace=indent,eol,start

" Render tabs, trailing spaces, and non-breaking spaces.
set list
set listchars=tab:>\ ,space:\ ,trail:-

" Pressing tab inserts four spaces.
" Hard tabs appear four spaces wide.
set tabstop=4
set shiftwidth=4
set expandtab

" Copy indent from current line when starting new line.
set autoindent

" Prevents exrc and other stuff from running potentially dangerous shell commands.
set secure

" Set scrolloff to a high value to keep the cursor centered.
set scrolloff=15
"set sidescrolloff=

" Enable syntax highlighting.
syntax on

" Detect filetype.
filetype plugin indent on

" The font for gvim.
set guifont=Inconsolata\ 12

"" Colorscheme
" Darker background.
let g:alduin_Shout_Dragon_Aspect = 1

" Use underline matchparens instead of block.
let g:alduin_Shout_Aura_Whisper = 1

" Disable string background highlight.
let g:alduin_Shout_Animal_Allegiance = 1

colo alduin

" Make the cursor number line the same as the others.
hi CursorLineNR guifg=#020202 guibg=#444444

" Red and black cursor.
hi Cursor guifg=#000000 guibg=#ff0000 ctermfg=0 ctermbg=9

" Load the GDB debugger integration plugin that comes with vim.
packadd termdebug

" Clear old mappings when sourcing this file.
mapclear

" Map Y to be consistent with C and D.
noremap Y y$

" Disable the annoying message in the command line when using Ctrl-c.
nnoremap <c-c> <silent> <c-c>

" Remap increment/decrement number to something more intuitive
noremap <c-a> +
noremap <c-x> -


" Closes the current buffer without closing the window.
" Also doesn't leave any [New File]s around.
" Provided by the vim-bbye plugin.
noremap <silent> <leader>d :up\|Bdelete<cr>
noremap <silent> <leader>D :Bdelete!<cr>

" Save and quit everything.
noremap <leader>q :wqa<cr>

" Write file with sudo permissions.
noremap <leader>W :w !sudo tee %<cr>
noremap <silent> <leader>w :up<cr>

" Open vertical terminal.
noremap <silent> <leader>t :vert term<cr>

" Don't let the integrated terminal interrupt buffer switching.
tnoremap <silent> <c-j> <c-\><c-n>:bn<cr>
tnoremap <silent> <c-k> <c-\><c-n>:bp<cr>

" Exit terminal mode easier.
tnoremap <c-q> <c-\><c-n><c-w>

" Rebind K to be a complement to J.
vnoremap K <esc>i<cr><esc>k$
noremap K i<cr><esc>k$

" Switch between buffers.
noremap <silent> <c-k> :bp<cr>
inoremap <silent> <c-k> <esc>:bp<cr>
noremap <silent> <c-j> :bn<cr>
inoremap <silent> <c-j> <esc>:bn<cr>

" Clear search buffer in normal mode when pressing escape<cr>.
nnoremap <silent> <esc> :noh<cr><esc>

" Session slots.
"TODO: predefined veriable representing the nvim directory?
noremap <leader>1 <esc>:wa\|mksession! ~/.config/nvim/sessions/1.vim<cr>
noremap <leader>! <esc>:so ~/.config/nvim/sessions/1.vim<cr>
noremap <leader>2 <esc>:wa\|mksession! ~/.config/sessions/2.vim<cr>
noremap <leader>@ <esc>:so ~/.config/nvim/sessions/2.vim<cr>
noremap <leader>3 <esc>:wa\|mksession! ~/.config/nvim/sessions/3.vim<cr>
noremap <leader># <esc>:so ~/.config/nvim/sessions/3.vim<cr>

" Source vimrc.
noremap <leader>- <esc>:source $MYVIMRC\|AirlineToggle\|AirlineToggle<cr>

" Edit vimrc.
noremap <leader>= <esc>:e $MYVIMRC<cr>

" Copy line (or selection) into command buffer and run it.
"nnoremap <leader>- yy:<c-r>"<bs><cr>
"vnoremap <leader>- y:<c-r>"<cr>

" Delete trailing whitespace in buffer. (Or selection).
nnoremap <leader>0 :%s/\s\+$//e\|noh\|up<cr>
vnoremap <leader>0 :s/\s\+$//e\|noh\|up<cr>

" Toggle spell checking locally.
nnoremap <leader>9 :setlocal spell!<cr>

" Toggle hard line wrapping.
command! HardWrapToggle if &fo =~ 't' | set fo-=t | echo 'Hard line wrap disabled' | else | set fo+=t | echo 'Hard line wrap enabled' | endif
nnoremap <leader>8 :HardWrapToggle<cr>

" Run rustfmt.
nnoremap <leader>7 :up\|RustFmt<cr>

" Search for file with fzf.
noremap <leader>f :Files<cr>

" Grep for string using ripgrep.
noremap <leader>s :Rg<cr>

" Search for open buffer.
noremap <leader>b :Buffers<cr>

" Search marks.
noremap <leader>m :Marks<cr>

" Search lines in open buffers.
noremap <leader>l :Lines<cr>

" Search v:oldfiles and open buffers.
noremap <leader>h :History<cr>

" Prompt for binary to start debugging.
noremap <leader>g <esc>:Termdebug<space>
noremap <leader>G <esc>:TermdebugCommand<space>

" How long to show highlighted yanked text (millis).
let g:highlightedyank_highlight_duration = 150

" Make the yanked region color the same as the alduin search color.
hi HighlightedyankRegion guifg=#dfdfaf guibg=#875f5f gui=NONE ctermfg=187 ctermbg=95 cterm=NONE

" Don't run zig fmt after saving.
let g:zig_fmt_autosave = 0

" Disable the abomination known as netrw.
let g:loaded_netrwPlugin = 1

" Show the list of open buffers and tabs at the top of the screen.
let g:airline#extensions#bufferline#enabled = 1
let g:airline#extensions#tabline#enabled = 1

" Modify the default ignored buffer names so that neovim's terminal shows up in the bufferline.
let g:airline#extensions#tabline#ignore_bufadd_pat = 'defx|gundo|nerd_tree|startify|tagbar|undotree|vimfiler'

" Make the bufferline prettier.
let g:airline#extensions#bufferline#left_sep = ' '
let g:airline#extensions#bufferline#right_sep = ' '
let g:airline#extensions#bufferline#left_alt_sep = ' '
let g:airline#extensions#bufferline#right_alt_sep = ' '

"Make the tabline prettier.
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#right_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = ' '
let g:airline#extensions#tabline#right_alt_sep = ' '

" On the tabline, only show the file name with the full path on the right.
let g:airline#extensions#tabline#formatter = 'unique_tail'

" Set Airline theme with the vim-airline-themes plugin.
let g:airline_theme = 'atomic'

" Make fzf match the color scheme (Doesn't fully work for :Rg).
let g:fzf_colors =
\ { 'fg': ['fg', 'Normal'],
\ 'bg': ['bg', 'Normal'],
\ 'hl': ['fg', 'Comment'],
\ 'fg+': ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
\ 'bg+': ['bg', 'CursorLine', 'CursorColumn'],
\ 'hl+': ['fg', 'Statement'],
\ 'info': ['fg', 'PreProc'],
\ 'border': ['fg', 'Ignore'],
\ 'prompt': ['fg', 'Conditional'],
\ 'pointer': ['fg', 'Exception'],
\ 'marker': ['fg', 'Keyword'],
\ 'spinner': ['fg', 'Label'],
\ 'header': ['fg', 'Comment'] }

" Language plugins like to set formatoptions, but I don't want them doing that.
" (See :h fo-table)
autocmd BufNewFile,BufRead * setlocal fo=q

" Save current view settings on a per-window, per-buffer basis.
function! AutoSaveWinView()
    if !exists("w:SavedBufView")
        let w:SavedBufView = {}
    endif
    let w:SavedBufView[bufnr("%")] = winsaveview()
endfunction

" Restore current view settings.
function! AutoRestoreWinView()
    let buf = bufnr("%")
    if exists("w:SavedBufView") && has_key(w:SavedBufView, buf)
        let v = winsaveview()
        let atStartOfFile = v.lnum == 1 && v.col == 0
        if atStartOfFile && !&diff
            call winrestview(w:SavedBufView[buf])
        endif
        unlet w:SavedBufView[buf]
    endif
endfunction

" When switching buffers, preserve window view.
if v:version >= 700
    autocmd BufLeave * call AutoSaveWinView()
    autocmd BufEnter * call AutoRestoreWinView()
endif

" Keep this.
:noh

