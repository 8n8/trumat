const test = require('ava');
const {exec} = require('../../setup');

test(
	'typecheck: inference across declarations catches an error in usage of a top-level function',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await t.throwsAsync(program);
		t.is(snapshot.code, 1);
		await t.context.cliSnapshot(snapshot);
	}
);
