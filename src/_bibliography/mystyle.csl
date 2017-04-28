<?xml version="1.0" encoding="utf-8"?>
<style xmlns="http://purl.org/net/xbiblio/csl" class="in-text" version="1.0" demote-non-dropping-particle="sort-only" default-locale="en-US">
  <info>
    <!--
    Personal style
    made by modifying http://www.zotero.org/styles/springer-lecture-notes-in-computer-science by A. Merari and M. Ronkko
    (retrieved on January 31st, 2016).
    Documentation for CSL files is here: http://docs.citationstyles.org/en/stable/
    -->
    <author>
      <name>Pascal Poizat</name>
      <email>pascal.poizat@gmail.com</email>
    </author>
    <category citation-format="numeric"/>
    <category field="engineering"/>
    <rights license="http://creativecommons.org/licenses/by-sa/3.0/">This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 License</rights>
  </info>
  <macro name="author">
    <group>
      <names variable="author">
        <name and="text", delimiter=", " delimiter-precedes-et-al="never", delimiter-precedes-last="contextual"/>
        <label form="short" prefix=" " strip-periods="true"/>
        <substitute>
          <names variable="editor"/>
        </substitute>
      </names>
    </group>
  </macro>
  <macro name="editor">
    <names variable="editor">
      <name initialize-with="." delimiter=", " and="text" name-as-sort-order="all" sort-separator=", "/>
      <label form="short" prefix=" (" suffix=")"/>
    </names>
  </macro>
  <macro name="title">
    <choose>
      <if type="bill book graphic legal_case legislation motion_picture report song" match="any">
        <text variable="title" font-weight="bold"/>
      </if>
      <else>
        <text variable="title" font-weight="bold"/>
      </else>
    </choose>
  </macro>
  <macro name="publisher">
    <text variable="publisher"/>
    <text variable="publisher-place" prefix=", "/>
  </macro>
  <macro name="page">
    <group>
      <label variable="page" form="short" suffix=" "/>
      <text variable="page"/>
    </group>
  </macro>
  <citation et-al-min="3" et-al-use-first="1" collapse="citation-number">
    <sort>
      <key variable="citation-number"/>
    </sort>
    <layout prefix="[" suffix="]" delimiter=", ">
      <text variable="citation-number"/>
    </layout>
  </citation>
  <bibliography entry-spacing="0" second-field-align="flush">
    <layout suffix=".">
      <!--<text variable="citation-number" suffix="."/>-->
      <text macro="author" suffix=". "/>
      <choose>
        <if type="bill book graphic legal_case legislation motion_picture report song" match="any">
          <group delimiter=" ">
            <text macro="title" suffix="."/>
            <text macro="publisher"/>
            <date variable="issued">
              <date-part name="year"/>
            </date>
          </group>
        </if>
        <else-if type="article-journal">
          <group delimiter=" ">
            <text macro="title" suffix="."/>
            <text variable="container-title" form="short" suffix="," font-style="italic"/>
            <group delimiter="">
            <text variable="volume"/>
            <text variable="issue" prefix="(" suffix=")"/>
            </group>
            <text variable="page" prefix=":" suffix=","/>
            <date variable="issued">
              <date-part name="year"/>
            </date>
          </group>
        </else-if>
        <else-if type="chapter paper-conference" match="any">
          <group delimiter=" ">
            <text macro="title" suffix="."/>
            <choose>
              <if variable="container-title">
                <text term="in" text-case="capitalize-first" suffix=": "/>
                <text macro="editor" suffix=" "/>
                <group delimiter=", ">
                  <text variable="container-title" font-style="italic"/>
                  <text macro="page"/>
                  <text macro="publisher" suffix=", "/>
                </group>
              </if>
              <else>
                <text term="presented at" text-case="capitalize-first" suffix=" "/>
                <text variable="event"/>
                <text variable="event-place" prefix=", "/>
                <date variable="issued" prefix=" ">
                  <date-part name="month" suffix=" "/>
                  <date-part name="day" suffix=" "/>
                </date>
              </else>
            </choose>
            <date variable="issued">
              <date-part name="year"/>
            </date>
          </group>
        </else-if>
        <else-if type="webpage">
          <group delimiter=", ">
            <text macro="title"/>
            <text variable="URL"/>
          </group>
        </else-if>
        <else-if type="speech">
          <group delimiter=" ">
            <text macro="title" suffix="."/>
            <text variable="event" suffix="."/>
            <text variable="event-place" prefix=", "/>
            <date variable="issued">
              <date-part name="year"/>
            </date>
          </group>
        </else-if>
        <else-if type="thesis" match="any">
            <group delimiter=", ">
                <text macro="title"/>
                <text variable="genre"/>
                <text variable="publisher"/>
                <group delimiter=" ">
                <date variable="issued">
                  <date-part name="month"/>
                </date>
                <date variable="issued">
                  <date-part name="year"/>
                </date>
                </group>
            </group>
        </else-if>
        <else>
          <group delimiter=", ">
            <text macro="title"/>
            <text variable="URL"/>
            <date variable="issued">
              <date-part name="year"/>
            </date>
          </group>
        </else>
      </choose>
    </layout>
  </bibliography>
</style>
