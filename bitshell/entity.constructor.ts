constructor Entity(did, identityType, role, region='auto-adapt', rightsProfile='delegable') {
  this.id = did
  this.identityType = identityType
  this.role = role
  this.region = region
  this.rightsProfile = rightsProfile
  this.complianceState = { consented: false, consentRevocable: true, lastUpdated: now() }
  this.context = { currentActivity: null, riskFlags: [], emotionIndex: 'neutral', volatilityLevel: 'gradual' }
}
