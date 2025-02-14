<?xml version="1.0" encoding="UTF-8"?>
<!--
	Transform Datacite xml to rdf/xml.
	Test result with:
	http://www.w3.org/RDF/Validator/
	http://www.easyrdf.org/converter (N.B. does not accept rdf:resource="" although this is legal)
	
	update 2015-11-17:
	 - correction in sf:Polygon (commas and spaces were reversed)
	update 2016-01-13:
	 - changed treatment of resourceType
	update 2016-02-16:
	 - creators/contributors: datacite individuals for orcid/isni and fabio:hasURL for other schemes
	 - all subjects as SKOS concepts
	updates 2016-02-25/2016-02-27:
	 - support for all nameIdentifierSchemes known as individuals in the Datacite ontology
	 - support for FunderIdentifier and FunderIdentifierScheme
	 - publicationYear as xs:gYear
	 - approptiate xs:* types for dates
	 - better whitespace cleaning
	 - language RFC5646
	 - better mapping for resourceType/resourceTypeGeneral, hasGeneralResourceType instead of rdf:type
	 - Indentifier(Scheme) replaced by ResourceIdentifier(Scheme) where appropriate
	 - rdfs:label replaced by dct:description in some places (language, identifiers)
	 - relatedIdentifiers: some new IdentifierSchemes and special treatment for 'IsIdenticalTo'
	 - relatedIdentifiers: revamped relatedMetadata stuff
	 - added SKOS concept schemes
	 - better test of string against IANA media types
	 - rights as dct:RightsStatement with litre:hasLiteralValue and fabio:hasURL
	 - description (simplified!) with description types and hasDesciption(Type) properties from Datacite ontology
	 - deprecated dc namespace (all dct now)
	 - fixed prioritization of some templates
	Now conforms to all of datacite2rdf document.
	
	2016-02-28 Egbert Gramsbergen
-->
<xsl:stylesheet version="2.0" exclude-result-prefixes="d3 fun"
	xmlns="http://purl.org/spar/datacite/"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:d3="http://datacite.org/schema/kernel-3"
	xmlns:owl="http://www.w3.org/2002/07/owl#"
	xmlns:dct="http://purl.org/dc/terms/"
	xmlns:dcmitype="http://purl.org/dc/dcmitype/"
	xmlns:foaf="http://xmlns.com/foaf/0.1/"
	xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
	xmlns:skos="http://www.w3.org/2004/02/skos/core#"
	xmlns:vcard="http://www.w3.org/2006/vcard/ns#"
	xmlns:wsg="http://www.w3.org/2003/01/geo/wgs84_pos#"
	xmlns:sf="http://www.opengis.net/ont/sf#"
	xmlns:ogc="http://www.opengis.net/ont/geosparql#"
	xmlns:litre="http://www.essepuntato.it/2010/06/literalreification/"
	xmlns:pro="http://purl.org/spar/pro/"
	xmlns:fabio="http://purl.org/spar/fabio/"
	xmlns:frapo="http://purl.org/cerif/frapo/"
	xmlns:cito="http://purl.org/spar/cito/"
	xmlns:doco="http://purl.org/spar/doco/"
	xmlns:deo="http://purl.org/spar/deo/"
	xmlns:c4o="http://purl.org/spar/c4o/"
	xmlns:vivo="http://vivoweb.org/ontology/core#"
	xmlns:co="http://purl.org/co/"
	xmlns:prism="http://prismstandard.org/namespaces/basic/3.0/"
	xmlns:frbr="http://purl.org/vocab/frbr/core#"
	xmlns:fun="funcions/"
	>
	
	<xsl:output method="xml" encoding="UTF-8" indent="yes"/>

	<!-- ==================== Global parameters and variables ==================== -->
	
	<xsl:param name="spar" select="'http://purl.org/spar/'"/>
	<xsl:param name="datacite" select="'http://purl.org/spar/datacite/'"/>
	<xsl:param name="fabio" select="'http://purl.org/spar/fabio/'"/>
	<xsl:param name="frbr" select="'http://purl.org/vocab/frbr/core#'"/>
	<xsl:param name="dcmitype" select="'http://purl.org/dc/dcmitype/'"/>
	<xsl:param name="mediatype" select="'https://w3id.org/spar/mediatype/'"/>
	<xsl:param name="doiResolver" select="'http://dx.doi.org/'"/>
	<xsl:param name="shortDoiResolver" select="'http://doi.org/'"/>
	<xsl:param name="handleResolver" select="'http://hdl.handle.net/'"/>
	<xsl:param name="arXivResolver" select="'http://arxiv.org/abs/'"/>
	<xsl:param name="datacitePersonalIdSchemes" select="
		'dia',
		'jst',
		'national-insurance-number',
		'nii',
		'openid',
		'orcid',
		'researcherid',
		'social-security-number',
		'viaf'"/>
	<xsl:param name="dataciteFunderIdSchemes" select="'fundref'"/>
	<xsl:param name="dataciteAgentIdSchemes" select="'isni'"/>
	
	<!-- debug true/false: copy/ignore elements for which no specific templates exist -->
	<xsl:param name="debug" select="false()"/>
	
	<!-- Prepare input for main template -->
	<xsl:variable name="in">
		<!-- Convert input datacite xml to v3 -->
		<xsl:variable name="temp">
			<xsl:choose>
				<xsl:when test="/d3:resource">
					<xsl:copy-of select="."/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:apply-templates select="." mode="copy2d3namespace"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<!-- Add/modify some attributes for later convenience an do some whitespace cleanup -->
		<xsl:apply-templates select="$temp" mode="addAttr"/>
	</xsl:variable>
	
	<xsl:variable name="doi" select="$in/d3:resource/d3:identifier[@identifierType='DOI']"/>
	
	<!-- ==================== Templates ==================== -->
	
	<!-- Copy everything -->
	<xsl:template match="@*|node()" mode="#default copy2d3namespace addAttr">
		<xsl:copy>
			<xsl:apply-templates select="@*|node()" mode="#current"/>
		</xsl:copy>
	</xsl:template>
	
	<!-- Normalize whitespace in attributes with default and xml namespaces -->
	<xsl:template match="@*[name()=local-name()]|@xml:*" mode="addAttr">
		<!-- name() is safe for default and xml namespaces -->
		<xsl:attribute name="{name()}" select="normalize-space(.)"/>
	</xsl:template>

	<!-- Normalize whitespace in most elements -->
	<xsl:template mode="addAttr" match="*[not(self::d3:description)]/text()">
		<xsl:value-of select="normalize-space(.)"/>
	</xsl:template>
	
	<!-- Eliminate unwanted whitespace -->
	<xsl:template match="text()[normalize-space(.)='']" mode="#default copy2d3namespace addAttr" priority="100"/>
	
	<!-- ==================== Templates preparing input $in ==================== -->
	
	<!-- Change namespace to Datacite v3 -->
	<xsl:template match="*" mode="copy2d3namespace">
		<xsl:element name="d3:{local-name()}">
			<xsl:apply-templates select="@*|node()" mode="#current"/>
		</xsl:element>
	</xsl:template>
	
	<!-- Add id to specific elements -->
	<xsl:template mode="addAttr" match="
			d3:alternateIdentifier|
			d3:creator|
			d3:contributor|
			d3:description|
			d3:format|
			d3:geoLocation|
			d3:relatedIdentifier|
			d3:rights|
			d3:size|
			d3:subject">
		<xsl:copy>
			<xsl:attribute name="id" select="concat('#',local-name(),xs:string(count(preceding-sibling::d3:*)+1))"/>
			<xsl:apply-templates select="@*|node()" mode="#current"/>
		</xsl:copy>
	</xsl:template>
	
	<!-- Normalize nameIdentifierScheme -->
	<xsl:template match="@nameIdentifierScheme" mode="addAttr" priority="200">
		<!-- schemeURI (optional) and nameIdentifierScheme (required) are free text, so we need some fuzzy orcid/isni detection -->
		<xsl:variable name="scheme" select="lower-case(.)"/>
		<xsl:variable name="schemeURI" select="../@schemeURI"/>
		<xsl:attribute name="{name()}" select="
			if (contains($schemeURI,'orcid.org') or (Sscheme='orcid')) then 'orcid'
			else if (contains($schemeURI,'isni.org') or (Sscheme='isni')) then 'isni'
			else ."/>
	</xsl:template>

	<!-- Construct Agent type and role from contributorType -->
	<xsl:template match="@contributorType" mode="addAttr">
		<!--
			issue:
			scoro/data-creator: "the role a person has of being a creator or originator of new data", but DataCollector may also be an organization
		-->
		<xsl:variable name="type-role" select="
			if (.='Other') then ('','')
			else if (.='ContactPerson') then ('foaf:Person','scoro/contact-person')
			else if (.='DataCollector') then ('','scoro/data-creator')
			else if (.='DataCurator') then ('foaf:Person','scoro/data-curator')
			else if (.='DataManager') then ('foaf:Person','scoro/data-manager')
			else if (.='Distributor') then ('','pro/distributor')
			else if (.='Editor') then ('','pro/editor')
			else if (.='Funder') then ('','scoro/funder')
			else if (.='HostingInstitution') then ('foaf:Organization','scoro/host-institution')
			else if (.='Producer') then ('','pro/producer')
			else if (.='ProjectLeader') then ('foaf:Person','scoro/project-leader')
			else if (.='ProjectManager') then ('foaf:Person','scoro/project-manager')
			else if (.='ProjectMember') then ('foaf:Person','scoro/project-member')
			else if (.='RegistrationAgency') then ('foaf:Organization','scoro/registar')
			else if (.='RegistrationAuthority') then ('foaf:Organization','scoro/registration-authority')
			else if (.='RelatedPerson') then ('foaf:Person','')
			else if (.='ResearchGroup') then ('frapo:ResearchGroup','')
			else if (.='RightsHolder') then ('','scoro/rights-holder')
			else if (.='Researcher') then ('foaf:Person','scoro/researcher')
			else if (.='Sponsor') then ('','scoro/sponsor')
			else if (.='Supervisor') then ('foaf:Person','scoro/supervisor')
			else if (.='WorkPackageLeader') then ('foaf:Person','scoro/workpackage-leader')
			else ('','')"/>
		<xsl:for-each select="$type-role[1][. ne '']">
			<xsl:attribute name="type" select="."/>
		</xsl:for-each>
		<xsl:for-each select="$type-role[2][. ne '']">
			<xsl:attribute name="role" select="."/>
		</xsl:for-each>
	</xsl:template>

	<!-- ==================== Root template ==================== -->
	
	<!-- Root template calls named-root template, providing a hook for testing with importing stylesheet -->
	<xsl:template match="/">
		<xsl:call-template name="named-root"/>
	</xsl:template>

	<!-- ==================== All templates below process input $in ==================== -->
	
	<!-- Catch prepared input and continue from there -->
	<xsl:template name="named-root">
		<!-- attribute value template does not work for @xml:base in Saxon 9.6.0.5. Workaround with xsl:attribute. -->
		<rdf:RDF>
			<xsl:attribute name="xml:base" select="concat($doiResolver,$doi)"/>
			<xsl:apply-templates select="$in/*"/>
		</rdf:RDF>
	</xsl:template>

	<!-- Main template describing the resource -->
	<xsl:template match="d3:resource">
		<rdf:Description rdf:about="">
			<xsl:apply-templates/>
		</rdf:Description>
		<xsl:call-template name="creatorList"/>
		<xsl:apply-templates select="//(d3:creator|d3:contributor)"/>
	</xsl:template>

	<!-- Transparent wrapper elements -->
	<xsl:template match="
			d3:alternateIdentifiers|
			d3:dates|d3:descriptions|
			d3:formats|
			d3:geoLocations|
			d3:relatedIdentifiers|
			d3:rightsList|
			d3:titles|
			d3:sizes|
			d3:subjects">
		<xsl:apply-templates select="*"/>
	</xsl:template>
	
	<!-- Debugging. If debugging is off, ignore elements for which no specific templates exist (otherwise copy) -->
	<xsl:template match="*[not($debug)]" priority="-0.4"/>
	
	<!-- ==================== Templates for creators and contributors ==================== -->
	
	<!-- Creator list -->
	<xsl:template name="creatorList">
		<xsl:for-each select="d3:creators">
			<co:List rdf:about="#creator-list">
				<co:firstItem rdf:resource="{d3:creator[1]/@id}"/>
				<co:lastItem rdf:resource="{d3:creator[last()]/@id}"/>
				<co:size rdf:datatype="xs:nonNegativeInteger">
					<xsl:value-of select="count(d3:creator)"/>
				</co:size>
			</co:List>
			<xsl:for-each select="d3:creator">
				<co:ListItem rdf:about="{@id}-list-item">
					<co:index rdf:datatype="xs:nonNegativeInteger">
						<xsl:value-of select="position()"/>
					</co:index>
					<co:itemContent rdf:resource="{@id}"/>
					<xsl:for-each select="following-sibling::d3:creator[1]">
						<co:nextItem rdf:resource="{@id}"/>
					</xsl:for-each>
				</co:ListItem>
			</xsl:for-each>
		</xsl:for-each>
	</xsl:template>
	
	<!-- Creator triples departing from "" -->
	<xsl:template match="d3:creators">
		<hasCreatorList rdf:resource="#creator-list"/>
		<xsl:for-each select="d3:creator">
			<dct:creator rdf:resource="{@id}"/>
		</xsl:for-each>
	</xsl:template>
	
	<!-- Contributor triples departing from "" -->
	<xsl:template match="d3:contributors">
		<xsl:for-each select="d3:contributor">
			<dct:contributor rdf:resource="{@id}"/>
		</xsl:for-each>
	</xsl:template>
	
	<!-- Creator/contributor resources (foaf:Agent or subclass) -->	
	<xsl:template match="d3:creator|d3:contributor">
		<xsl:element name="{(@type, if (d3:nameIdentifier/@nameIdentifierScheme=$datacitePersonalIdSchemes) then 'foaf:Person' else 'foaf:Agent')[1]}">
			<xsl:attribute name="rdf:about" select="@id"/>
			<xsl:apply-templates select="@role|*"/>
		</xsl:element>
	</xsl:template>

	<!-- for creators and contributors: name -->
	<xsl:template match="d3:creatorName|d3:contributorName">
		<foaf:name>
			<xsl:value-of select="."/>
		</foaf:name>
	</xsl:template>

	<!-- for creators and contributors: name identifiers -->
	<xsl:template match="d3:nameIdentifier">
		<xsl:variable name="type" select="@nameIdentifierScheme"/>
		<xsl:variable name="hasDatacitePersonalScheme" select="$type=$datacitePersonalIdSchemes"/>
		<xsl:variable name="hasDataciteFunderScheme" select="$type=$dataciteFunderIdSchemes"/>
		<xsl:variable name="hasDataciteAgentScheme" select="$hasDatacitePersonalScheme or $hasDataciteFunderScheme or $type=$dataciteAgentIdSchemes"/>
		<xsl:variable name="scheme" select="if ($hasDataciteAgentScheme) then concat($datacite,@nameIdentifierScheme) else @schemeURI"/>
		<xsl:variable name="id" select="if ($type=('orcid','isni')) then tokenize(.,'/')[last()] else ."/>
		<xsl:variable name="idid" select="concat(../@id,'-id')"/>
		<xsl:if test="$type='orcid'">
			<foaf:homepage rdf:resource="http://orcid.org/{$id}"/>
		</xsl:if>
		<hasIdentifier>
			<xsl:element name="{if ($hasDatacitePersonalScheme) then 'PersonalIdentifier' else if ($hasDataciteFunderScheme) then 'FunderIdentifier' else 'AgentIdentifier'}">
				<xsl:attribute name="rdf:about" select="$idid"/>
				<litre:hasLiteralValue>
					<xsl:value-of select="$id"/>
				</litre:hasLiteralValue>
				<xsl:choose>
					<xsl:when test="$hasDataciteAgentScheme">
						<!-- No need to add the labels for these schemes because they are known in the ontology -->
						<usesIdentifierScheme rdf:resource="{$scheme}"/>
					</xsl:when>
					<xsl:otherwise>
						<usesIdentifierScheme>
							<AgentIdentifierScheme rdf:about="{$idid}-scheme">
								<xsl:for-each select="$scheme[. ne '']">
									<fabio:hasURL rdf:datatype="xs:anyURI">
										<xsl:value-of select="."/>
									</fabio:hasURL>
								</xsl:for-each>
								<skos:prefLabel>
									<xsl:value-of select="$type"/>
								</skos:prefLabel>
							</AgentIdentifierScheme>
						</usesIdentifierScheme>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:element>
		</hasIdentifier>
	</xsl:template>
	
	<!-- for creators and contributors: affiliation -->
	<xsl:template match="d3:affiliation">
		<pro:holdsRoleInTime>
			<xsl:variable name="id" select="../@id"/>
			<pro:RoleInTime rdf:about="{$id}-affiliation">
				<pro:relatesToEntity rdf:resource=""/>
				<pro:withRole rdf:resource="{$spar}pro/affiliate"/>
				<pro:relatesToOrganization>
					<foaf:Organization rdf:about="{$id}-affiliated-org">
						<foaf:name>
							<xsl:value-of select="."/>
						</foaf:name>
					</foaf:Organization>
				</pro:relatesToOrganization>
			</pro:RoleInTime>
		</pro:holdsRoleInTime>
	</xsl:template>
	
	<!-- contributor role -->
	<xsl:template match="@role">
		<pro:holdsRoleInTime>
			<pro:RoleInTime rdf:about="{../@id}-role">
				<pro:relatesToEntity rdf:resource=""/>
				<pro:withRole rdf:resource="{$spar}{.}"/>
			</pro:RoleInTime>
		</pro:holdsRoleInTime>
	</xsl:template>
	
	<!-- ==================== Geo templates ==================== -->
	
	<!-- geoLocation -->
	<xsl:template match="d3:geoLocation">
		<dct:spatial>
			<ogc:Feature rdf:about="{@id}">
				<xsl:apply-templates/>
			</ogc:Feature>
		</dct:spatial>
	</xsl:template>
	
	<!-- geoLocationBox -->
	<xsl:template match="d3:geoLocationBox">
		<!--
			A latlonbox is approximated by a polygon.
			This is not exact but the lat deviations can be made negligible by splitting up lon into small enough segments:
				(1) d = arctan(tan(y)/cos(w/2))-y
			where (all in radians):
				d = max deviation in longitude
				w = width of segment
				y = longitude
			For w<<1, (1) reduces to
				 (2) d = 1/8 w^2 sin(y)cos(y)
			or
				 (3) d =< 1/16 w^2
			where the equality holds for y = pi/4.
			Filling in practical values we see that the max north-south error is
			- 100m for a 1 deg segment (O(100km) wide at moderate y)
			- 40km for a box spanning half the earth (180 deg) split up in 10 segments.
			This leads to our criteria: split latitude in segments of approx W=1 degree but max N=10 segments.
		-->
		<xsl:param name="W" select="1."/>
		<xsl:param name="N" select="10"/>
		<!-- 2*(lat long) -->
		<xsl:variable name="coords" select="tokenize(.,'\s')"/>
		<xsl:if test="count($coords)=4">
			<xsl:variable name="z" select="for $c in $coords return xs:double($c)"/>
			<xsl:variable name="y" select="$z[position()=(1,3)]"/>
			<xsl:variable name="x" select="$z[position()=(2,4)]"/>
			<xsl:variable name="error" select="$x[abs(.) gt 180.], $y[abs(.) gt 90.]"/>
			<xsl:if test="not($error[1])">
				<xsl:variable name="crossesMeridian180" select="abs($x[0]-$x[1]) gt 180."/>
				<xsl:variable name="x0" select="if ($crossesMeridian180) then max($x) else min($x)"/>
				<xsl:variable name="x1" select="if ($crossesMeridian180) then min($x) else max($x)"/>
				<xsl:variable name="w" select="$x1 - $x0 + (if ($crossesMeridian180) then 360. else 0.)"/>
				<xsl:variable name="n" select="min(($N,$w idiv $W + 1))"/>
				<xsl:variable name="d" select="$w div $n"/>
				<xsl:variable name="xseq" select="for $i in (0 to $n) return $x1+$d*$i"/>
				<xsl:variable name="west-east" select="for $z in $xseq return xs:string(if ($z gt 180.) then $z -360. else $z)"/>
				<xsl:variable name="east-west" select="for $i in (0 to $n) return $west-east[$n+1-$i]"/>
				<xsl:variable name="north" select="xs:string(max($y))"/>
				<xsl:variable name="south" select="xs:string(min($y))"/>
				<!-- closed loop, clockwise -->
				<xsl:variable name="points" select="
					concat($west-east[1],' ',$south),
					for $s in $west-east return concat($s,' ',$north),
					for $s in $east-west return concat($s,' ',$south)"/>
				<xsl:variable name="wkt" select="concat('POLYGON((',string-join($points,','),'))')"/>
				<ogc:hasGeometry>
					<sf:Polygon rdf:about="{../@id}-box">
						<ogc:asWKT rdf:datatype="ogc:wktLiteral">
							<xsl:value-of select="$wkt"/>
						</ogc:asWKT>
					</sf:Polygon>
				</ogc:hasGeometry>
			</xsl:if>
		</xsl:if>
	</xsl:template>
	
	<!-- geoLocationPoint -->
	<xsl:template match="d3:geoLocationPoint">
		<!-- lat long -->
		<xsl:variable name="coords" select="tokenize(.,'\s')"/>
		<xsl:if test="count($coords)=2">
			<!-- long lat -->
			<xsl:variable name="z" select="for $c in $coords return abs(xs:double($c))"/>
			<xsl:variable name="error" select="$z[1][. gt 180.], $z[2][. gt 90.]"/>
			<xsl:if test="not($error[1])">
				<xsl:variable name="wkt" select="concat('POINT(',$coords[2],' ',$coords[1],')')"/>
				<ogc:hasGeometry>
					<sf:Point rdf:about="{../@id}-point">
						<ogc:asWKT rdf:datatype="ogc:wktLiteral">
							<xsl:value-of select="$wkt"/>
						</ogc:asWKT>
					</sf:Point>
				</ogc:hasGeometry>
			</xsl:if>
		</xsl:if>
	</xsl:template>
	
	<!-- <geoLocationPlace> -->
	<xsl:template match="d3:geoLocationPlace">
		<rdfs:label>
			<xsl:value-of select="."/>
		</rdfs:label>
	</xsl:template>
	
	<!-- ==================== relatedIdentifier ==================== -->
	
	<!-- relatedIdentifier -->
	<xsl:template match="d3:relatedIdentifier">
		<xsl:variable name="relationTypes">
			<relation type="IsCitedBy"><cito:isCitedBy/></relation>
			<relation type="Cites"><cito:cites/></relation>
			<relation type="IsSupplementTo"><frbr:supplementOf/></relation>
			<relation type="IsSupplementedBy"><frbr:supplement/></relation>
			<relation type="IsContinuedBy"><frbr:successor/></relation>
			<relation type="Continues"><frbr:successorOf/></relation>
			<relation type="HasMetadata"><cito:citesAsMetadataDocument/></relation>
			<relation type="IsMetadataFor"><cito:isCitedAsMetadataDocumentBy/></relation>
			<relation type="IsNewVersionOf"><frbr:revisionOf/></relation>
			<relation type="IsPreviousVersionOf"><frbr:revision/></relation>
			<relation type="IsPartOf"><dct:isPartOf/></relation>
			<relation type="HasPart"><dct:hasPart/></relation>
			<relation type="IsReferencedBy"><cito:isCitedForInformationBy/></relation>
			<relation type="References"><cito:citesForInformation/></relation>
			<relation type="IsDocumentedBy"><cito:isDocumentedBy/></relation>
			<relation type="Documents"><cito:documents/></relation>
			<relation type="IsCompiledBy"><cito:isCompiledBy/></relation>
			<relation type="Compiles"><cito:compiles/></relation>
			<relation type="IsVariantFormOf"><frbr:arrangementOf/></relation>
			<relation type="IsOriginalFormOf"><frbr:arrangement/></relation>
			<relation type="IsReviewedBy"><cito:isReviewedBy/></relation>
			<relation type="Reviews"><cito:reviews/></relation>
			<relation type="IsDerivedFrom"><cito:citesAsDataSource/></relation>
			<relation type="IsSourceOf"><cito:isCitedAsDataSourceBy/></relation>
			<relation type=""><dct:relation/></relation>
		</xsl:variable>
		<!--
		see https://schema.datacite.org/meta/kernel-3.1/index.html
		@relatedIdentifierType - all have an equivalent (lower-case) insividual in the Datacite ontology.
		Values with * are (resolved to) urls.
			ARK
			arXiv *
			bibcode
			DOI *
			EAN13
			EISSN
			Handle *
			ISBN
			ISSN
			LISSN
			LSID
			ISTC
			PMID
			PURL *
			UPC
			URL *
			URN
		-->
		<xsl:variable name="this" select="."/>
		<xsl:variable name="idType" select="@relatedIdentifierType"/>
		<xsl:variable name="relType" select="@relationType"/>
		<xsl:for-each select="($relationTypes/*[@type=($relType,'')]/*)[1]">
			<xsl:copy>
				<xsl:for-each select="$this">
					<xsl:variable name="url" select="(
						fun:valid_url_value(.),
						fun:valid_url_value(
							if ($idType='arXiv') then concat($arXivResolver,replace(.,'^arxiv:','','i')) else
							if ($idType='DOI') then concat($doiResolver,replace(.,'^doi:','','i')) else
							if ($idType='Handle') then concat($handleResolver,.) else ()),
						@id)[1]"/>
					<xsl:variable name="isURL" select="$idType='URL' and not(starts-with($url,'#'))"/>
					<xsl:choose>
						<xsl:when test="$isURL and not(@relatedMetadataScheme|@schemeURI|@schemeType)">
							<xsl:attribute name="rdf:resource" select="$url"/>
						</xsl:when>
						<xsl:otherwise>
							<rdf:Description rdf:about="{$url}">
								<xsl:if test="not($isURL)">
									<hasIdentifier>
										<ResourceIdentifier rdf:about="{@id}-id">
											<litre:hasLiteralValue>
												<xsl:value-of select="."/>
											</litre:hasLiteralValue>
											<usesIdentifierScheme rdf:resource="{$datacite}{lower-case($idType)}"/>
										</ResourceIdentifier>
									</hasIdentifier>
								</xsl:if>
								<xsl:if test="$relType='HasMetadata'">
									<xsl:call-template name="relatedMetadataDetails"/>
								</xsl:if>
							</rdf:Description>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:for-each>
			</xsl:copy>
		</xsl:for-each>
		<xsl:if test="@relationType='IsMetadataFor'">
			<xsl:call-template name="relatedMetadataDetails"/>
		</xsl:if>
	</xsl:template>
	
	<!-- relatedIdentifier: @relatedMetadataScheme, @schemeURI, @schemeType -->
	<xsl:template name="relatedMetadataDetails">
		<usesMetadataScheme>
			<MetadataScheme rdf:about="{@id}scheme">
				<xsl:if test="@relatedMetadataScheme">
					<rdfs:label>
						<xsl:value-of select="@relatedMetadataScheme"/>
					</rdfs:label>
					<xsl:for-each select="@schemeURI">
						<fabio:hasURL rdf:datatype="xs:anyURI">
							<xsl:value-of select="."/>
						</fabio:hasURL>
					</xsl:for-each>
				</xsl:if>
				<xsl:for-each select="@schemeType">
					<dct:format>
						<xsl:choose>
							<xsl:when test="fun:isMediaType(.)">
								<xsl:attribute name="rdf:resource" select="concat($datacite,.)"/>
							</xsl:when>
							<xsl:otherwise>
								<rdf:Description rdf:about="{../@id}format">
									<rdfs:label>
										<xsl:value-of select="."/>
									</rdfs:label>
								</rdf:Description>
							</xsl:otherwise>
						</xsl:choose>
					</dct:format>
				</xsl:for-each>
			</MetadataScheme>
		</usesMetadataScheme>
	</xsl:template>

	<!-- relatedIdentifier: special case 'IsIdenticalTo' -->
	<xsl:template match="d3:relatedIdentifier[@relationType='IsIdenticalTo']">
		<xsl:variable name="type" select="@relatedIdentifierType"/>
		<xsl:choose>
			<xsl:when test="$type='URL' and fun:valid_url($type)">
				<owl:sameAs rdf:resource="{.}"/>
			</xsl:when>
			<xsl:otherwise>
				<hasIdentifier>
					<AlternateResourceIdentifier rdf:about="{@id}">
						<litre:hasLiteralValue>
							<xsl:value-of select="."/>
						</litre:hasLiteralValue>
						<usesIdentifierScheme rdf:resource="{$datacite}{lower-case($type)}"/>
					</AlternateResourceIdentifier>
				</hasIdentifier>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- ==================== Other templates ==================== -->
	
	<!-- alternateIdentifier -->
	<xsl:template match="d3:alternateIdentifier">
		<xsl:choose>
			<xsl:when test="lower-case(@alternateIdentifierType)='url' and fun:valid_url(.)">
				<owl:sameAs rdf:resource="{.}"/>
			</xsl:when>
			<xsl:otherwise>
				<hasIdentifier>
					<AlternateResourceIdentifier rdf:about="{@id}">
						<litre:hasLiteralValue>
							<xsl:value-of select="."/>
						</litre:hasLiteralValue>
						<usesIdentifierScheme rdf:about="{@id}Scheme">
							<ResourceIdentifierScheme>
								<dct:description>
									<xsl:value-of select="@alternateIdentifierType"/>
								</dct:description>
							</ResourceIdentifierScheme>
						</usesIdentifierScheme>
					</AlternateResourceIdentifier>
				</hasIdentifier>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<!-- date -->
	<xsl:template match="d3:date">
		<!-- What to do with Collected? -->
		<xsl:variable name="granularity" select="
				if (. castable as xs:gYear) then 'xs:gYear'
				else if (. castable as xs:date) then 'xs:date'
				else  if (. castable as xs:dateTime) then 'xs:dateTime'
				else ()"/>
		<xsl:if test="$granularity">
			<xsl:variable name="type" select="
					if (@dateType='Accepted') then 'dct:dateAccepted'
					else if (@dateType='Available') then 'dct:available'
					else if (@dateType='Collected') then 'fabio:hasDateCollected'
					else if (@dateType='Copyrighted') then 'dct:dateCopyrighted'
					else if (@dateType='Created') then 'dct:created'
					else if (@dateType='Issued') then 'dct:issued'
					else if (@dateType='Submitted') then 'dct:dateSubmitted'
					else if (@dateType='Updated') then 'dct:modified'
					else if (@dateType='Valid') then 'dct:valid'
					else 'dct:date'"/>
			<xsl:element name="{$type}">
				<xsl:attribute name="rdf:datatype" select="$granularity"/>
				<xsl:value-of select="."/>
			</xsl:element>
		</xsl:if>
	</xsl:template>
	
	<!-- description -->
	<xsl:template match="d3:description">
		<!-- The description may contain d3:br elements. These are handled by fun:text() -->
		<hasDescription>
			<rdf:Description rdf:about="{@id}">
				<litre:hasLiteralValue>
					<xsl:copy-of select="@lang,fun:text(.)"/>
				</litre:hasLiteralValue>
				<xsl:for-each select="@descriptionType">
					<hasDescriptionType rdf:resource="{concat($datacite,
							if (.='Abstract') then 'abstract'
							else if (.='Methods') then 'methods'
							else if (.='SeriesInformation') then 'series-information'
							else if (.='TableOfContents') then 'table-of-contents'
							else 'other')}"/>
				</xsl:for-each>
			</rdf:Description>
		</hasDescription>
	</xsl:template>
		
	<!-- format -->
	<xsl:template match="d3:format">
		<!-- format is free text. Try to map to a media type, use as its label otherwise. -->
		<dct:format>
			<xsl:choose>
				<xsl:when test="fun:isMediaType(text())">
					<xsl:attribute name="rdf:resource" select="concat($mediatype,.)"/>
				</xsl:when>
				<xsl:otherwise>
					<dct:MediaTypeOrExtent rdf:about="{@id}">
						<rdfs:label>
							<xsl:value-of select="."/>
						</rdfs:label>
					</dct:MediaTypeOrExtent>
				</xsl:otherwise>
			</xsl:choose>
		</dct:format>
	</xsl:template>

	<!-- identifier -->
	<xsl:template match="d3:identifier">
		<hasIdentifier>
			<PrimaryResourceIdentifier rdf:about="#primaryIdentifier">
				<litre:hasLiteralValue><xsl:value-of select="$doi"></xsl:value-of></litre:hasLiteralValue>
				<usesIdentifierScheme rdf:resource="{$datacite}doi"/>
			</PrimaryResourceIdentifier>
		</hasIdentifier>
	</xsl:template>

	<!-- language -->
	<xsl:template match="d3:language">
		<!--
			RFC5646 allows subcodes
			(unlike e.g. http://www.lexvo.org/id/iso639-1/en and http://www.lexvo.org/id/iso639-3/eng)
		-->
		<dct:language>
			<dct:LinguisticSystem rdf:about="#language">
				<dct:description rdf:datatype="dct:RFC5646">
					<xsl:value-of select="."/>
				</dct:description>
			</dct:LinguisticSystem>
		</dct:language>
	</xsl:template>

	<!-- publicationYear -->
	<xsl:template match="d3:publicationYear">
		<xsl:if test=". castable as xs:gYear">
			<fabio:hasPublicationYear rdf:datatype="xs:gYear">
				<xsl:value-of select="."/>
			</fabio:hasPublicationYear>
		</xsl:if>
	</xsl:template>

	<!-- publisher -->
	<xsl:template match="d3:publisher">
		<dct:publisher>
			<foaf:Agent rdf:about="#publisher">
				<foaf:name>
					<xsl:value-of select="."/>
				</foaf:name>
			</foaf:Agent>
		</dct:publisher>
	</xsl:template>

	<!-- resourceType -->
	<xsl:template match="d3:resourceType">
		<!--
			resourceTypeGeneral is picked from a list which maps to resources.
			resourceType is free text, in practice often only different from resourceTypeGeneral in capitailzation and/or whitespace.
			If so, it is mapped to the same resource as resourceTypeGeneral.
			If not, we just use its value as the label of some unknown class.
			More sophisticated mapping may be added in the future.
		-->	
		<xsl:variable name="RTgenParts" select="fun:class_gen(@resourceTypeGeneral)"/>
		<xsl:variable name="RTgen" select="string-join($RTgenParts,'')"/>
		<rdf:type>
			<xsl:choose>
				<xsl:when test="translate(lower-case(.),' ','')=lower-case($RTgenParts[2])">
					<xsl:attribute name="rdf:resource" select="$RTgen"/>
				</xsl:when>
				<xsl:otherwise>
					<owl:Class rdf:about="#resourceType">
						<rdfs:label><xsl:value-of select="."/></rdfs:label>
					</owl:Class>
				</xsl:otherwise>
			</xsl:choose>
		</rdf:type>
		<hasGeneralResourceType rdf:resource="{$RTgen}"/>
	</xsl:template>

	<!-- rights -->
	<xsl:template match="d3:rights">
		<dct:rights>
			<dct:RightsStatement rdf:about="{@id}">
				<litre:hasLiteralValue>
					<xsl:value-of select="."/>
				</litre:hasLiteralValue>
				<xsl:for-each select="@rightsURI">
					<fabio:hasURL rdf:datatype="xs:anyURI">
						<xsl:value-of select="."/>
					</fabio:hasURL>
				</xsl:for-each>
			</dct:RightsStatement>
		</dct:rights>
	</xsl:template>

	<!-- size -->
	<xsl:template match="d3:size">
		<!-- Lacking detailed knowledge about meaning and content, we use dct:extent and values are just strings.	-->
		<dct:extent>
			<dct:SizeOrDuration rdf:about="{@id}">
				<rdf:value>
					<xsl:value-of select="."/>
				</rdf:value>
			</dct:SizeOrDuration>
		</dct:extent>
	</xsl:template>
	
	<!-- subject -->
	<xsl:template match="d3:subject">
		<!--
			In d3, a subject can have a subjectScheme and a schemeUri (both optional and free text).
			There is no distinction between the code and the label. We guess the pattern as <code><whitespace><label>.
			Use dct:subject or a fabio sub-property linking to a SKOS Concept, either existing (only Dewey) or made ad-hoc.
		-->
		<xsl:variable name="attsVal" select="string-join(for $a in (@subjectScheme,@schemeURI) return lower-case($a),' ')"/>
		<xsl:variable name="text" select="text()"/>
		<xsl:variable name="code" select="substring-before($text,' ')"/>
		<xsl:variable name="type" select="if (contains($attsVal,'dewey') and matches($code,'^\d{3}[\.\d+]?$')) then 'dewey' else ()"/>
		<xsl:variable name="label" select="if ($type) then substring-after($text,' ') else $text"/>
		<xsl:variable name="relation" select="
				if (not($type)) then 'dct:subject'
				else if (contains($code,'.')) then 'fabio:hasSubjectTerm'
				else 'fabio:hasSubjectDiscipline'"/>
		<xsl:element name="{$relation}">
			<skos:Concept rdf:about="{if ($type) then concat($spar,'dewey/',$code) else @id}">
				<!-- skos:prefLabel would be desirable but we cannot be sure -->
				<rdfs:label>
					<xsl:copy-of select="@xml:lang,$label"/>
				</rdfs:label>
				<!-- The following is not necessary for existing SKOS concept because it can all be found there -->
				<xsl:if test="not($type) and (@subjectScheme or @schemURI)">
					<!-- Better reuse of ad-hoc schemes within a record may be implemented in the future -->
					<skos:inScheme>
						<skos:ConceptScheme rdf:about="{@id}-scheme">
							<xsl:for-each select="@subjectScheme">
								<rdfs:label>
									<xsl:value-of select="."/>
								</rdfs:label>
							</xsl:for-each>
							<xsl:for-each select="@schemeURI">
								<fabio:hasURL rdf:datatype="xs:anyURI">
									<xsl:value-of select="."/>
								</fabio:hasURL>
							</xsl:for-each>
						</skos:ConceptScheme>
					</skos:inScheme>
				</xsl:if>
			</skos:Concept>
		</xsl:element>
	</xsl:template>

	<!-- title -->
	<xsl:template match="d3:title">
		<xsl:element name="{
			if (@titleType='AlternativeTitle') then 'dct:alternative'
			else if (@titleType='Subtitle') then 'fabio:hasSubtitle'
			else if (@titleType='TranslatedTitle') then 'fabio:hasTranslatedTitle'
			else 'dct:title'}">
			<xsl:copy-of select="@xml:lang|text()"/>
		</xsl:element>
	</xsl:template>
	
	<!-- version -->
	<xsl:template match="d3:version">
		<prism:versionIdentifier>
			<xsl:value-of select="."/>
		</prism:versionIdentifier>
	</xsl:template>
	
	<!-- ==================== Functions ==================== -->
	
	<!-- function fun:valid_url: lightweight url validation -->
	<xsl:function name="fun:valid_url">
		<xsl:param name="url"/>
		<xsl:variable name="regex" select="'^(https?|ftp)://[^\s/$.?#].[^\s]*$'"/>
		<xsl:sequence select="matches($url,$regex,'i')"/>
	</xsl:function>

	<!-- function fun:valid_url_value: returns valid url or nothing -->
	<xsl:function name="fun:valid_url_value">
		<xsl:param name="url"/>
		<xsl:sequence select="if (fun:valid_url($url)) then $url else ()"/>
	</xsl:function>
	
	<!-- function fun:text: replace element in mixed content by line break -->
	<xsl:function name="fun:text">
		<xsl:param name="in"/>
		<xsl:sequence select="string-join((for $i in $in/node() return if ($i/self::text()) then $i else '&#xA;'),'')"/>
	</xsl:function>
	
	<!-- function fun:class_gen: get class from text in ResourceTypeGeneral (sequence of namespace, local part) -->
	<xsl:function name="fun:class_gen">
		<xsl:param name="text"/>
		<xsl:sequence select="
			if ($text=(
				'Collection',
				'Dataset',
				'Event',
				'InteractiveResource',
				'PhysicalObject',
				'Service',
				'Software',
				'Sound',
				'Text'
				)) then ($dcmitype,$text)
			else if ($text='Audiovisual') then ($dcmitype,'MovingImage')
			else if ($text='Image') then ($dcmitype,'StillImage')
			else if ($text=('Model','Workflow')) then ($fabio,$text)
			else ($frbr,'Endeavour')"/>
	</xsl:function>
	
	<!-- function fun:isMediaType: test if string is an IANA media type -->
	<xsl:function name="fun:isMediaType">
		<xsl:param name="text"/>
		<xsl:variable name="main" select="(
			'application',
			'audio',
			'image',
			'message',
			'model',
			'multipart',
			'text',
			'video')"/>
		<xsl:variable name="parts" select="tokenize($text,'/')"/>
		<xsl:variable name="result">
			<xsl:choose>
				<xsl:when test="count($parts)!=2"/>
				<xsl:when test="not($parts[1]=$main)"/>
				<xsl:when test="tokenize($parts[2],'\+')[3]"/>
				<xsl:when test="matches($parts[2],'^[-+.\w]+$')">y</xsl:when>
			</xsl:choose>
		</xsl:variable>
		<xsl:sequence select="$result='y'"/>
	</xsl:function>
	
</xsl:stylesheet>