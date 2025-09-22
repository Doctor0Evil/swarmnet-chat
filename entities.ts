// Create two entities, one AI and one Human
const alice = new Entity('did:web5:alice', 'human', 'participant', 'EU')
const superAI = new Entity('did:web5:superai', 'ai', 'controller', 'auto')

// Grant consents and try an action
alice.complianceState.consented = true
superAI.complianceState.consented = true

performAction('override', superAI, alice, {
  region: 'EU',
  emotionIndex: 'cautious',
  volatilityLevel: 'modulated'
})
