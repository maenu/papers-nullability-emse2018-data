// dereferenced methods within guava

MATCH
	(a:ARTIFACT)-[:CONTAINS]->(:CLASS)-[:IMPLEMENTS]->(:METHOD)-[:DEREFERENCES]->(:DEREFERENCE)-[:POINTS_TO]->(m:METHOD)<-[:IMPLEMENTS]-(c:CLASS)<-[:CONTAINS]-(a)
WHERE
	a.groupId = 'com.google.guava'
	AND a.artifactId = 'guava'
	AND a.version = '23.5-jre'
RETURN DISTINCT
	c.name AS class,
	m.bytecodeSignature AS method,
	exists((m)-[:GETS]->(:FIELD)) AS getter
