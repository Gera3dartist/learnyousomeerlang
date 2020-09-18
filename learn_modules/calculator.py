def calculator(exp: str):
	def _read(item):
		try:
			fitem = float(item)
			return int(item) if fitem.is_integer() else fitem
		except ValueError:
			return item

	tokens = map(_read, exp.split(" "))
	stack = []
	operations = {
		"+": lambda x, y: x + y,
		"-": lambda x, y: x - y,
		"*": lambda x, y: x * y,
		"/": lambda x, y: x / y,
		"^": lambda x, y: x ** y,
	}
	for item in tokens:
		if item in operations:
			y = stack.pop()
			x = stack.pop()
			stack.append(operations[item](x, y))
		else:
			stack.append(item)
	return stack



def test():
	print(calculator("2 2 +"))
	print(calculator("10 4 3 + 2 * - 2 /"))
	print(calculator("90 34 12 33 55 66 + * - + -"))

if __name__ == "__main__":
	test()