// dereferences within guava

MATCH
	(a:ARTIFACT)-[:CONTAINS]->(c:CLASS)-[:IMPLEMENTS]->(m:METHOD)-[:DEREFERENCES]->(d:DEREFERENCE)-[p:POINTS_TO]->(dm:METHOD)<-[:DECLARES]-(dc:CLASS)<-[:CONTAINS]-(a)
WHERE
	a.groupId = 'com.google.guava'
	AND a.artifactId = 'guava'
	AND a.version = '23.5-jre'
WITH
	a,
	c,
	m,
	d,
	p,
	dm,
	dc,
	rand() AS r
WHERE
	p.nullness = 'UNKNOWN'
RETURN
	c.name AS class,
	m.bytecodeSignature AS method,
	d.variable AS variable,
	d.sourceLocation AS location,
	p.nullness AS nullness,
	dc.name AS originClass,
	dm.bytecodeSignature AS originMethod,
	r
ORDER BY
	r
LIMIT
	33
UNION ALL
MATCH
	(a:ARTIFACT)-[:CONTAINS]->(c:CLASS)-[:IMPLEMENTS]->(m:METHOD)-[:DEREFERENCES]->(d:DEREFERENCE)-[p:POINTS_TO]->(dm:METHOD)<-[:DECLARES]-(dc:CLASS)<-[:CONTAINS]-(a)
WHERE
	a.groupId = 'com.google.guava'
	AND a.artifactId = 'guava'
	AND a.version = '23.5-jre'
WITH
	a,
	c,
	m,
	d,
	p,
	dm,
	dc,
	rand() AS r
WHERE
	p.nullness = 'NON_NULL'
RETURN
	c.name AS class,
	m.bytecodeSignature AS method,
	d.variable AS variable,
	d.sourceLocation AS location,
	p.nullness AS nullness,
	dc.name AS originClass,
	dm.bytecodeSignature AS originMethod,
	r
ORDER BY
	r
LIMIT
	33
UNION ALL
MATCH
	(a:ARTIFACT)-[:CONTAINS]->(c:CLASS)-[:IMPLEMENTS]->(m:METHOD)-[:DEREFERENCES]->(d:DEREFERENCE)-[p:POINTS_TO]->(dm:METHOD)<-[:DECLARES]-(dc:CLASS)<-[:CONTAINS]-(a)
WHERE
	a.groupId = 'com.google.guava'
	AND a.artifactId = 'guava'
	AND a.version = '23.5-jre'
WITH
	a,
	c,
	m,
	d,
	p,
	dm,
	dc,
	rand() AS r
WHERE
	p.nullness = 'NULL'
RETURN
	c.name AS class,
	m.bytecodeSignature AS method,
	d.variable AS variable,
	d.sourceLocation AS location,
	p.nullness AS nullness,
	dc.name AS originClass,
	dm.bytecodeSignature AS originMethod,
	r
ORDER BY
	r
LIMIT
	33
