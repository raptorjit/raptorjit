local ffi = require("ffi")

do --- byte array allocations
  local typ = ffi.typeof"uint8_t[?]"
  for i = 4, 24 do
    for d = -5, 5 do
      local sz = 2^i + d
      assert(ffi.sizeof(typ, sz) == sz)
      local mem = ffi.new(typ, sz)
      assert(ffi.sizeof(mem) == sz)
      mem[0] = 0x21
      mem[1] = 0x32
      mem[2] = 0x43
      mem[sz-3] = 0x54
      mem[sz-2] = 0x65
      mem[sz-1] = 0x76
      assert(mem[0] == 0x21)
      assert(mem[1] == 0x32)
      assert(mem[2] == 0x43)
      assert(mem[3] == 0)
      assert(mem[sz-4] == 0)
      assert(mem[sz-3] == 0x54)
      assert(mem[sz-2] == 0x65)
      assert(mem[sz-1] == 0x76)
    end
  end
end

do --- int array allocations
  local typ = ffi.typeof"int32_t[?]"
  for i = 2, 17 do
    for d = -2, 2 do
      local sz = 2^i + d
      assert(ffi.sizeof(typ, sz) == sz*4)
      local mem = ffi.new(typ, sz)
      assert(ffi.sizeof(mem) == sz*4)
      mem[0] = -3
      mem[sz-1] = -4
      assert(mem[0] == -3)
      if sz ~= 2 then
        assert(mem[1] == 0)
        assert(mem[sz-2] == 0)
      end
      assert(mem[sz-1] == -4)
    end
  end
end

do --- ISEQS
   local s = ffi.new("char[123]")
   assert(s ~= "foo", "s ~= foo")
   local ms = ffi.metatype(
      "struct { char s[123]; }",
      {__eq = function (x, y) return ffi.string(x.s) == y end}
   )
   local s1 = ms{s="foo"}
   assert(s1 == "foo", "s1 == foo (__eq)")
   assert(s1 ~= "bar", "s1 ~= bar (__eq)")
end

do --- ISEQP
   local s = ffi.typeof("struct { bool b; }")
   local s1 = s{b=true}
   assert(s1 ~= true,  "s1 ~= true")
   local ms = ffi.metatype(s, {__eq = function (x, y) return x.b == y end})
   local s2 = s{b=true}
   assert(s1 == true,  "s2 == true")
   assert(s1 ~= false,  "s2 ~= false")
   assert(s1 ~= nil,  "s2 ~= nil")
end
