function map<A, B = A>(mapper: (item: A, index: number, array: A[]) => B) {
	return (list: A[]) => list.map(mapper);
}

const max = (arr: number[]) => arr.reduce((a, n) => (a > n ? a : n), -Infinity);
const min = (arr: number[]) => arr.reduce((a, n) => (a < n ? a : n), Infinity);

const toArray = (spaceDelin: string): string[] => spaceDelin.split(' ');
const toNumber = (str: string) => parseInt(str, 10);

export const Kata = {
	highAndLow(numStr: string): string {
		const numArray = map(toNumber)(toArray(numStr));
		return `${max(numArray)} ${min(numArray)}`;
	},
};
