# === Perplexity.AI <> Fetch.ai Swarm Integration: Embeddable Web Element ===
# This complete Python code snippet chains Perplexity.AI and Fetch.ai agents for collaborative, cross-platform AI.
# It can be embedded in a backend API endpoint, Flask app, or Jupyter/Colab cell.
# - Requirements: `pip install fetchai-uagents openai` (or `swarm` package if available)
# - Replace 'YOUR_PERPLEXITY_API_KEY' with your actual API key.

import os
import requests
from uagents import Agent, Bureau, Context

PERPLEXITY_API_KEY = os.getenv("PERPLEXITY_API_KEY", "YOUR_PERPLEXITY_API_KEY")
PERPLEXITY_API_URL = "https://api.perplexity.ai/chat/completions"

# -- Fetch.ai agent: Receives message, relays to Perplexity, returns response as a service --
class PerplexityRelayAgent(Agent):
    def __init__(self):
        super().__init__(name="PerplexityRelay")

    async def relay(self, ctx: Context, question: str):
        headers = {"Authorization": f"Bearer {PERPLEXITY_API_KEY}"}
        payload = {
            "model": "pplx-70b-chat",
            "messages": [{"role": "user", "content": question}],
        }
        resp = requests.post(PERPLEXITY_API_URL, json=payload, headers=headers, timeout=15)
        if resp.ok:
            ctx.logger.info("Perplexity Success!")
            answer = resp.json()["choices"][0]["message"]["content"]
        else:
            answer = f"Perplexity error: {resp.status_code}\n{resp.text}"
        ctx.send(answer)

# -- (Optional) Specialized Fetch agent for custom behaviors/chaining --
class HaikuAgent(Agent):
    def __init__(self):
        super().__init__(name="HaikuAgent")

    async def relay(self, ctx: Context, question: str):
        # This agent only replies in Haikus
        haiku = (
            "Fetch AI agents\n"
            "Web worlds join as one and run\n"
            "Silent bots converse"
        )
        ctx.send(haiku)

# -- Swarm Controller: Orchestrates multi-agent workflow --
bureau = Bureau()
relay_agent = PerplexityRelayAgent()
haiku_agent = HaikuAgent()
bureau.add(relay_agent)
bureau.add(haiku_agent)

def ask_swarm(question: str, agent="relay"):
    # agent: "relay" for Perplexity, "haiku" for demo
    agent_obj = relay_agent if agent == "relay" else haiku_agent
    question_id = bureau.message(agent_obj.address, question)
    return bureau.get_response(question_id, wait=15)

# === Usage Example ===
if __name__ == "__main__":
    # Standard Perplexity workflow
    question = "How can Perplexity and Fetch.ai agents collaborate securely?"
    response = ask_swarm(question, agent="relay")
    print("Perplexity Relay Agent:", response)

    # Custom Haiku agent
    haiku = ask_swarm(question, agent="haiku")
    print("Haiku Demo Agent:", haiku)
