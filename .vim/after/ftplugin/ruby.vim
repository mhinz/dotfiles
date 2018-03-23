let v = readfile($HOME.'/.rbenv/version')[0]
if v != 'system'
  let &l:tags =
        \ printf('%s/.rbenv/versions/%s/lib/ruby/gems/%s/gems/*/tags,',$HOME,v,v).
        \ printf('%s/.rbenv/versions/%s/lib/ruby/%s/tags,',$HOME,v,v).
        \ printf('%s/.rbenv/versions/%s/lib/ruby/site_ruby/%s/tags,',$HOME,v,v).
        \ printf('%s/.rbenv/versions/%s/include/ruby-%s/tags',$HOME,v,v)
  let &l:path =
        \ printf('%s/.rbenv/versions/%s/lib/ruby/gems/%s/gems/*/lib,',$HOME,v,v).
        \ printf('%s/.rbenv/versions/%s/lib/ruby/%s,',$HOME,v,v).
        \ printf('%s/.rbenv/versions/%s/lib/ruby/%s/lib,',$HOME,v,v).
        \ printf('%s/.rbenv/versions/%s/lib/ruby/site_ruby/%s/lib,',$HOME,v,v).
        \ printf('%s/.rbenv/versions/%s/include/ruby-%s/tags',$HOME,v,v)
endif

let gemfile = findfile('Gemfile', '.;')
if !empty(gemfile)
  let lib = fnamemodify(gemfile, ':p:h').'/lib'
  if isdirectory(lib)
    let &l:path = lib .','. &l:path
  endif
endif

command! -buffer Dirs
      \ echomsg 'tags:' | exe 'PP split(&l:tags, ",")' |
      \ echomsg 'path:' | exe 'PP split(&l:path, ",")'
