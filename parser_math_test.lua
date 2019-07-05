function f(...)
if select('#', ...) == 1 then
	return (...)
else
	return "***"
end
end
assert(not('a'<'a') and ('a'<'b') and not('b'<'a'))
