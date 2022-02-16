# Text Mining with R

Diferentes abordagens para organizar e analisar dados da variedade de texto (livros, artigos, documentos). Também é fornecida uma cartilha sobre expressões regulares e maneiras de pesquisar padrões comuns no texto com eficiência. Estima-se que mais de 70% dos dados de negócios potencialmente utilizáveis não sejam estruturados. A mineração de texto fornece uma coleção de técnicas que nos permite obter insights de dados não estruturados.

---

## Modelagem de Tópicos: Alocação de Dirichlet Latente (LDA)

O modelo de tópico de alocação de Dirichlet latente ou *Latent Dirichlet allocation* (LDA) procura padrões de palavras que ocorrem juntas dentro e através de um corpus. O LDA encontra tópicos em um corpus criando um pacote separado para cada documento e despejando as palavras para procurar padrões nos quais as palavras aparecem juntas - não apenas em um pacote, mas consistentemente em todos os pacotes específicos de documento em nosso corpus. 

-Classificação de documentos em grupos;
-Reconhecimento do assunto principal e secundários em cada grupo;
-Classificação não boleana mas fuzzy;
-Útil para descobrir temas principais de textos.


> Observe que o LDA não está tentando explicar ou prever nenhuma variável dependente, como em uma regressão. Em vez disso, está simplesmente procurando padrões dentro de um grupo de variáveis explicativas. Essa busca de padrões é conhecida como aprendizado não supervisionado.

Os tópicos em si são uma lista de todas as palavras no corpus, muitas vezes referido como um dicionário, com probabilidades de cada palavra aparecer dentro de cada tópico. Palavras que aparecem frequentemente juntas terão alta probabilidade de ocorrer em um ou mais tópicos. 

Existem muitos modelos para realizar este tipo de análise, além do LDA, dentre os quais podemos listar Latent Semantic Analysis (LSA/LSI), Probabilistic Latent Semantic Analysis (pLSA) e a Non Negative Matrix Factorization (NNMF)

Vocabulário de NLP:

-Uma coleção de documentos é conhecido como *Corpus*

-*Bag-of-words* é tratar cada palavra em um documento, separadamente.

Em um data.frame arrumado cada linha é uma única palavra ou token usado em cada documento. No entanto, para executar um modelo de tópico, primeiro precisamos criar uma matriz de termos de documento ou DTM.

Uma matriz de termos de documento tem uma única linha para cada documento e uma coluna para cada palavra ou termo exclusivo usado em todos os documentos do corpus. Os valores no DTM são a contagem de tokens ou usos de cada termo para um determinado documento. Quando você tem uma matriz composta principalmente de zeros, isso é chamado de esparsidade ou matriz esparsa. Nosso DTM provavelmente será esparso, já que a maioria dos documentos não usa a maioria dos termos presentes no corpus.

-Segundo o LDA, o corpus é resultado de um processo generativo.

-Cada documento é uma mistura de K assuntos.

-Cada assunto possui uma distribuição de probabilidade para os V termos do vocabulário.

-Os tópicos são distribuições de probabilidade sobre o amplo vocabulário hipotético.

-Com esse modelo, em hipótese, se gera os documentos caso fossem conhecidos os parâmetros.

-É um modelo baseado em probabilidade condicional.











