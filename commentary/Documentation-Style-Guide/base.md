# Base documentation style guide

This document is a living, incremental style guide targeted at contributors to the `base` library's documentation. Its first and main purpose is to standardise our current practices when it comes to documentation writing.

This guide is not a language guide on the English language. The writer should refer to the New Oxford Style Manual, the Chicago Manual of Style, or any other professional reference for any concerns pertaining to writing in the English language.

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL
NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and
"OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).

## Table of contents

[[_TOC_]]

## British vs. American spelling

Being an international community of contributors, many of whom coming from English-speaking countries, there are bound to be different spelling standards living with each-other.  

* If editing an existing document, consistency must be favoured. If the document is inconsistent, an arbitrary spelling can be picked in order to harmonise the document.
* When creating a new document, the kind of spelling should be the one the author(s) are most familiar with, as to avoid potential irregularities.

## Greek letters for types, Latin letters for terms

The usage of "Greek letters for types, Latin letters for terms", whilst in use in some circles, is not as widespread as one may think. In particular, it can be harder for the reader to deal with another alphabet if Latin is not their first one. Moreover, it can potentially come off as pedantic (an image which our community struggles to avoid).  
Thus, this style is discouraged in the GHC documentation.

## Module documentation

Module documentation should contain an `Overview` section that describes in greater length the purpose, APIs and underlying concepts exposed by the module.
This section is typically not rendered through GHCi's `:doc` due to the particularities of a REPL's environment. That is why modules should always have a smaller, more compact introductory / summary paragraph that will be the first point of contact with the developer when they look modules up in the REPL.

## Sources

### Papers

References to published papers should contain a direct URL to the document, or if unavailable, to the paper's page on the publisher website. It is discouraged to reference papers that are behind paywalls. The Digital Object Identifier (DOI) may be given as well in order to enable the reader's ability to find the paper online should the provided URL suffer from bitrot.