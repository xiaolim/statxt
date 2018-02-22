struct test {
	int i;
	int j;
	float f;
};

int main()
{
	struct test s1;
	int i;

	i = 2;

	s1.i = 1;
	s1.j = i;
	s1.f = 3.4;

	return 0;
}
