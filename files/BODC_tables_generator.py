import os
import json
import pykg2tbl as kg
import pathlib
import pandas as pd
import requests
import numpy as np
import ast

# SPARQL EndPoint to use - wrapped as Knowledge-Graph 'source'
# SPARQL setup to use the NVS API
NSV_ENDPOINT: str = "https://vocab.nerc.ac.uk/sparql/sparql"
NSV:kg.KGSource = kg.KGSource.build(NSV_ENDPOINT)

TEMPLATES_FOLDER = str(pathlib.Path("files/nvsSPARQL-main/templated-queries/").absolute())
GENERATOR = kg.DefaultSparqlBuilder(templates_folder=TEMPLATES_FOLDER)

def generate_sparql(name: str, **vars) -> str: 
    """ Simply build the sparql by using the named query and applying the vars
    """
    return GENERATOR.build_syntax(name, **vars)

def execute_to_df(name: str, **vars) -> pd.DataFrame:
    """ Builds the sparql and executes, returning the result as a dataframe.
    """
    sparql = generate_sparql(name, **vars)
    result: kg.QueryResult = NSV.query(sparql=sparql)
    return result.to_dataframe()

valuesCollectionList=['L22','L05','F02','C17','S11','S10','S09','M20','M21','M24','L06']
parametersCollectionList=['Q01','P01','P02','P35']

# File paths
checkpoint_path = 'files/'

if os.path.exists(checkpoint_path+'BODCunits.csv'):
				print('BODCunits table already found, delete this version if you want to download a new one')
else:
				BODCunits=execute_to_df("nsv-listing.sparql", cc="P06")
				BODCunits=BODCunits[['id','pref_lang','alt','depr','member']]
				BODCunits.columns=['identifier','preflabel','altLabel','deprecated','uri']
				BODCunits.to_csv(checkpoint_path+'BODCunits.csv',index=False)

if os.path.exists(checkpoint_path+'BODCvalues.csv'):
				print('BODCvalues table already found, delete this version if you want to download a new one')
else:
				BODCvalues=pd.DataFrame(columns=['pref_lang','depr','member','definition'])
				for collection in valuesCollectionList :
					BODCvaluesTmp=execute_to_df("nsv-listing.sparql", cc=collection)
					for rowNumber in range(BODCvaluesTmp.shape[0]):
								try :
										definitionDict=ast.literal_eval(BODCvaluesTmp['definition'][rowNumber])
										try:
											BODCvaluesTmp.loc[rowNumber,'definition']=str(definitionDict['node'])
										except:
											BODCvaluesTmp.loc[rowNumber,'definition']='Unavailable'
								except:
										print('no dictionnary')
					BODCvalues=pd.concat([BODCvalues,BODCvaluesTmp[['pref_lang','depr','member','definition']]])
				BODCvalues.columns=['preflabel','deprecated','uri','definition']  
				url = "https://dd.eionet.europa.eu/vocabulary/biodiversity/eunishabitats/json"
				filename="eunisValues.json"
				response = requests.get(url)
				if response.status_code == 200:
						with open(checkpoint_path+filename, "wb") as f:
								f.write(response.content)

				with open(checkpoint_path+filename, 'r',encoding='utf-8') as file:
					eunisValues=json.load(file)

				for concept in eunisValues['concepts']:
						if concept['Status'] == 'Valid':
							newRow={'preflabel':concept['Label'],
								'deprecated':'FALSE',
								'uri':concept['skos:exactMatch'][0],
								'definition':concept['Definition']}
						else :
							newRow={'preflabel':concept['Label'],
								'deprecated':'TRUE',
								'uri':concept['skos:exactMatch'][0],
								'definition':concept['Definition']}
						BODCvalues=pd.concat([BODCvalues,pd.DataFrame.from_dict([newRow])])
				BODCvalues=BODCvalues.reset_index()      
				BODCvalues=BODCvalues.drop(columns='index')  
				BODCvalues.to_csv(checkpoint_path+'BODCvalues.csv',index=False)

if os.path.exists(checkpoint_path+'BODCparameters.csv'):
				BODCparameters = pd.read_csv(checkpoint_path+'BODCparameters.csv')
else:
				BODCparameters = pd.DataFrame(columns=['id','pref_lang','alt','depr','member','definition','standardUnitID'])
				columns_list=['id','pref_lang','alt','depr','member','definition','standardUnitID']
				for collection in parametersCollectionList :
								columns_list_copy=['id','pref_lang','alt','depr','member','definition','standardUnitID']
								BODCparametersTmp=execute_to_df("nsv-listing.sparql", cc=collection)
								for col in columns_list :
												if col not in BODCparametersTmp.columns :
																columns_list_copy.remove(col)
								BODCparameters=pd.concat([BODCparameters,BODCparametersTmp[columns_list_copy]])
				BODCparameters.columns=['identifier','preflabel','altLabel','deprecated','uri','definition','standardUnitID']   
				BODCparameters=BODCparameters.reset_index()
				BODCparameters=BODCparameters.drop(columns='index')
				BODCparameters.insert(len(BODCparameters.columns),"standardunit",np.nan)
				BODCparameters=BODCparameters.astype('object',copy=False,errors='ignore')

for rowNumber in range(BODCparameters.shape[0]):
				if type(BODCparameters['standardUnitID'][rowNumber]) != float :
								if type(BODCparameters['standardunit'][rowNumber]) == float :
												query_with_pref_lang = """
												PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

												SELECT ?pref_lang WHERE {
												<%s> skos:prefLabel ?pref_lang .
												}
												"""%BODCparameters['standardUnitID'][rowNumber]

												result: kg.QueryResult = NSV.query(sparql=query_with_pref_lang)
												BODCparameters.loc[rowNumber,'standardunit']=result.to_dict()['pref_lang'][0]
				if (rowNumber + 1) % 10000 == 0:
								BODCparameters.to_csv(checkpoint_path+'BODCparameters.csv', index=False)

rowNumber=BODCparameters.shape[0]
BODCparameters.loc[rowNumber,'identifier']="eunishabitats"
BODCparameters.loc[rowNumber,'preflabel']="EUNIS habitats"
BODCparameters.loc[rowNumber,'definition']="Classification of habitat types according to the EUNIS Biodiversity database"
BODCparameters.loc[rowNumber,'deprecated']="false"
BODCparameters.loc[rowNumber,'uri']="http://dd.eionet.europa.eu/vocabulary/biodiversity/eunishabitats/"

# Final save
BODCparameters.to_csv(checkpoint_path+'BODCparameters.csv', index=False)