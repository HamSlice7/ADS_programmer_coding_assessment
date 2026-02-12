# Clinical Trial Data Agent

- To run the Clinical Trial Data Agent Script, run the following command:
`uv run scripts/agent.py`

- This will prompt the agent with three example questions and print out three filtered AE SDTM data set data sets


## Note
Make sure to create a `.env` folder containing the following:
- OPENAI_API_KEY:  OPENAI API key 
- AE_SDTM_FILE_PATH: file path for a AE ADTM data set (use `pharmaversesdtmae.csv` for a test data set)
- SYSTEM_PROMPT_PATH: file path for the Clinical Trial Data Agent system prompt (use `prompts/AE_SDTM_prompt.txt` for the default system prompt)