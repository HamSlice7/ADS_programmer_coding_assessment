from dotenv import load_dotenv
from langchain_openai import ChatOpenAI
from langchain_core.prompts import ChatPromptTemplate
import os
import pandas as pd
import json

# Load environment variables from .env
load_dotenv()

system_prompt_path = os.getenv("SYSTEM_PROMPT_PATH")

ae_sdtm_file_path = os.getenv("AE_SDTM_FILE_PATH")

def ClinicalTrialDataAgent (user_question:str) -> dict:

    """
    Function that takes a user question about the AE SDTM dataset and uses and LLM to
    return a target column and filter value
    
    :param user_question: Questions about the AE SDTM dataset
    :type user_question: str
    :return: JSON return containing a target column and filter value for a AE SDTM data set
    :rtype: dict
    """ 

    # Initialize LLM
    llm = ChatOpenAI(model="gpt-4.1-mini",temperature=0)

    #Create prompt template with system prompt
    prompt = ChatPromptTemplate.from_messages([
        ("system", open(system_prompt_path).read()),
        ("human", "{question}")
    ])

    # Building a LLM prompt chain
    chain = prompt | llm

    # Get response from LLM
    response = chain.invoke({"question": user_question})

    return(json.loads(response.content))

def Convert_to_pandas_query(ae_path: pd.DataFrame, json_query:dict) -> pd.DataFrame:
    """
    Function that takes the JSON ouput from ClinicalTrialDataAgent to return a filtered
    AE SDTM data set
    
    :param ae_path: Path to the AE SDTM data set
    :type ae_path: pd.DataFrame
    :param json_query: ClinicalTrialDataAgent JSON ouput
    :type json_query: dict
    :return: Filtered dataframe 
    :rtype: DataFrame
    """
    
    #Read in AE SDTM data set as a pandas data frame
    df = pd.read_csv(ae_path)

    #Extract target column and filter value from LLM response
    target_column = json_query.get("target_column")
    filter_value = json_query.get("filter_value")

    #Filter AE SDTM data set based using target column and filter value from LLM response
    filtered_df = df[df[target_column].str.upper() == filter_value.upper()]
    
    return(filtered_df)

    
#Runnning three example queries 

test_question = [
    "Give me the subjects who had adverse events of moderate severity",
    "Give me the subjects whose adverse event resulted in death",
    "Give me the subjects whose adverse event was pruritus"
]


for question in test_question:
    response = ClinicalTrialDataAgent(question)
    filtered_df = Convert_to_pandas_query(ae_sdtm_file_path, response)
    unique_ids = filtered_df['USUBJID'].unique()
    print(f"Test question: {question}")
    print(f"Number of unique subjects after filtering: {len(unique_ids)}")
    print(f"Corresponding ID's: {unique_ids}")
    print("\n\n\n")