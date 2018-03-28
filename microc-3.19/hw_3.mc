int main () {
	f(27);

	return 0;
}

int f (int n) {
	while (n != 1) {
		print(n);

		if (n != ((n/2) * 2))
			n = 3 * n + 1;
		else
			n = n/2;
	}

	return 0;
}

