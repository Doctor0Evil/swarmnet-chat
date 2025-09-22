import requests
import json
from datetime import datetime

class NanoswarmSandbox:
    def __init__(self, api_token):
        self.api_token = api_token
        self.audit_log = []
        self.base_url = 'https://checkserp.com/api/v1'

    def encode_to_ascii(self, text):
        payload = {
            "api_token": self.api_token,
            "data": {"plain_text": text}
        }
        headers = {"Content-Type": "application/json"}
        r = requests.post(f'{self.base_url}/ascii-encode', data=json.dumps(payload), headers=headers)
        self.log_action("encode", text, r.status_code, r.text)
        self.enforce_intent_and_safety(r)
        return r.json()

    def log_action(self, action, input_val, status, output):
        self.audit_log.append({
            'action': action,
            'input': input_val,
            'status': status,
            'output': output,
            'timestamp': str(datetime.now())
        })

    def enforce_intent_and_safety(self, response):
        # Block, rollback, log or alert on dangerous/rogue patterns
        if 'unauthorized' in response.text or response.status_code != 200:
            raise Exception('Sandbox triggered! Bad intent or unsafe output blocked.')

    def snapshot_state(self):
        # Save asset/workflow state for rollback/recovery
        pass

# Usage
sandbox = NanoswarmSandbox(api_token='your-token-here')
ascii_art = sandbox.encode_to_ascii("Prototype asset tree")
print(ascii_art)
