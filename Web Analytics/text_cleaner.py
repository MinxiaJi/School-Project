
# coding: utf-8

# In[ ]:

from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize, RegexpTokenizer
from nltk.stem import PorterStemmer


# In[ ]:

def cleanText(raw_text):
    word_tokens = word_tokenize(raw_text.lower)
    tokenizer = RegexpTokenizer(r'\w+')
    clean_text = tokenizer.tokenize('',join(word_tokens))
    stop_words = set(stopwords.words('english'))
    cleaned_text = [w for w in clean_text if not w in stop_words]
    stemmer = PorterStemmer()
    clean_text = [stemmer.stem(t) for t in clean_text]
    clean_text = ''.join(clean_text)
    
    return clean_text

