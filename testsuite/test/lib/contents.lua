local function check(m, expected, exclude)
  local t = {}
  local ex = {}
  if exclude then
    for k in exclude:gmatch"[^:]+" do
      ex[k] = true
    end
  end
  for k in pairs(m) do
    if not ex[k] then
      t[#t+1] = tostring(k)
    end
  end
  table.sort(t)
  local got = table.concat(t, ":")
  if got ~= expected then
    error("got: \""..got.."\"\nexpected: \""..expected.."\"", 2)
  end
end

do --- base
  check(_G, "_G:_VERSION:arg:assert:collectgarbage:coroutine:debug:dofile:error:getmetatable:io:ipairs:load:loadfile:math:next:os:package:pairs:pcall:print:rawequal:rawget:rawset:require:select:setmetatable:string:table:tonumber:tostring:type:xpcall", "rawlen:bit:bit32:jit:gcinfo:setfenv:getfenv:loadstring:unpack:module:newproxy")
end

do --- pre-5.2 base +lua<5.2
  assert(gcinfo)
  assert(setfenv)
  assert(getfenv)
  assert(loadstring)
  assert(unpack)
  assert(module)
  assert(newproxy)
end

do --- 5.2 base +lua>=5.2
  assert(not gcinfo)
  assert(not setfenv)
  assert(not getfenv)
  assert(not loadstring)
  assert(not unpack)
  assert(not module)
  assert(not newproxy)
end

do --- pre-5.2 base rawlen -compat5.2
  assert(not rawlen)
end

do --- 5.2 base rawlen +compat5.2
  assert(rawlen)
end

do --- math
  check(math, "abs:acos:asin:atan:atan2:ceil:cos:cosh:deg:exp:floor:fmod:frexp:huge:ldexp:log:max:min:modf:pi:pow:rad:random:randomseed:sin:sinh:sqrt:tan:tanh", "log10:mod")
end

do --- pre-5.2 math +lua<5.2 -compat5.2
  assert(math.log10)
end

do --- pre-5.2 math +lua<5.2 -compat5.2 -luajit
   assert(math.mod)
end

do --- 5.2 math +lua>=5.2
  assert(not math.mod)
  assert(not math.log10)
end

do --- string
  check(string, "byte:char:dump:find:format:gmatch:gsub:len:lower:match:rep:reverse:sub:upper", "gfind")
end

do --- pre-5.2 string +lua<5.2 -compat5.2 -luajit
  assert(string.gfind)
end

do --- 5.2 string +lua>=5.2
  assert(not string.gfind)
end

do --- pre-5.2 table +lua<5.2
  check(table, "concat:foreach:foreachi:getn:insert:maxn:move:remove:sort", "pack:unpack:setn:new")
end

do --- 5.2 table +lua>=5.2
  check(table, "concat:insert:pack:remove:sort:unpack")
end

do --- pre-5.2 table.pack -compat5.2
  assert(not table.pack)
  assert(not table.unpack)
end

do --- 5.2 table.pack +compat5.2
  assert(table.pack)
  assert(table.unpack)
end

do --- io
  check(io, "close:flush:input:lines:open:output:popen:read:stderr:stdin:stdout:tmpfile:type:write")
end

do --- io file
  check(debug.getmetatable(io.stdin), "__gc:__index:__tostring:close:flush:lines:read:seek:setvbuf:write")
end

do --- os
  check(os, "clock:date:difftime:execute:exit:getenv:remove:rename:setlocale:time:tmpname")
end

do --- debug
  check(debug, "debug:gethook:getinfo:getlocal:getmetatable:getregistry:getupvalue:sethook:setlocal:setmetatable:setupvalue:traceback", "getfenv:setfenv:upvalueid:upvaluejoin:getuservalue:setuservalue")
end

-- TODO: Check versional differences in debug library

do --- package
  check(package, "config:cpath:loaded:loadlib:path:preload", "searchpath:loaders:searchers:seeall")
end

do --- pre-5.2 package +lua<5.2
  assert(package.loaders)
  assert(not package.searchers)
  assert(package.seeall)
end

do --- 5.2 package +lua>=5.2
  assert(not package.loaders)
  assert(package.searchers)
  assert(not package.seeall)
end

do --- package.loaders
  check(package.loaders or package.searchers, "1:2:3:4")
end

do --- package.loaded
  local loaded = {}
  for k, v in pairs(package.loaded) do
    if type(k) ~= "string" or (k:sub(1, 7) ~= "common." and k:sub(1, 4) ~= "jit.") then
      loaded[k] = v
    end
  end
  check(loaded, "_G:coroutine:debug:io:math:os:package:string:table", "bit:bit32:common:ffi:jit:table.new")
end

do --- bit +bit
  check(bit, "arshift:band:bnot:bor:bswap:bxor:lshift:rol:ror:rshift:tobit:tohex")
end

do --- ffi +ffi
  check(require"ffi", "C:abi:alignof:arch:cast:cdef:copy:errno:fill:gc:istype:load:metatype:new:offsetof:os:sizeof:string:typeof", "typeinfo")
end

do --- ffi 2.1 +ffi +luajit>=2.1
  assert(require"ffi".typeinfo)
end
