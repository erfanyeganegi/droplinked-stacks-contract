import { Cl } from '@stacks/transactions'
import { describe, expect, it } from 'vitest'

describe("'set-droplinked-admin' function", () => {
	it('ensures only current admin can call function', () => {
		const response = simnet.callPublicFn(
			'droplinked-operator',
			'set-droplinked-admin',
			[Cl.principal('ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5')],
			'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5'
		)

		expect(response.result).toBeErr(Cl.uint(100))
	})

	it('ensures current admin call function and change admin', () => {
		const response = simnet.callPublicFn(
			'droplinked-operator',
			'set-droplinked-admin',
			[Cl.principal('ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5')],
			'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM'
		)

		expect(response.result).toBeOk(Cl.bool(true))

		const adminResponse = simnet.callReadOnlyFn(
			'droplinked-operator',
			'get-droplinked-admin',
			[],
			'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM'
		)

		expect(adminResponse.result).toBePrincipal(
			'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5'
		)
	})
})