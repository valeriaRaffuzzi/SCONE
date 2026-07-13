.. _governance:

SCONE Governance
================

Purpose
-------

This document defines the governance of SCONE. Its purpose is to ensure that decisions regarding 
the development, maintenance, and direction of SCONE are made transparently and consistently. 
Unless explicitly amended according to the procedures below, this document governs all technical 
decisions related to SCONE.

Maintainers
-----------

SCONE is governed by a group of Maintainers. At the adoption of this document, the Maintainers are:

* Mikolaj Kowalski
* Valeria Raffuzzi
* Paul Cosgrove

Maintainers have write and administrative access to the SCONE GitHub repository. Maintainers have 
equal authority. No Maintainer has unilateral authority over technical decisions. Maintainers are 
responsible for:

* reviewing and merging pull requests;
* maintaining code quality and documentation;
* planning releases;
* making architectural decisions;
* resolving technical disputes.

A Maintainer is considered Active if they have participated in the governance or technical maintenance 
of SCONE within the preceding year. Participation includes activities such as reviewing pull requests, 
voting on governance matters, preparing releases, or making code contributions. A Maintainer who has not
participated during this period is considered Inactive until they resume participation. Unless otherwise 
specified, all voting thresholds in this document are calculated using the number of Active Maintainers.

Contributors
------------

Anyone may contribute to SCONE by submitting issues, pull requests, or suggest improvements. 
Contributors do not become Maintainers solely by making contributions, regardless of the size or 
importance of those contributions. Maintainer status is governed exclusively by Section 8.

Voting
------
  
Each active Maintainer has one vote. Silence does not constitute approval unless a voting deadline
has been explicitly announced. Votes should be recorded publicly whenever practical. An action is 
passed if approved by at least two-thirds of the active Maintainers. If two-thirds of the active 
Maintainers is not an integer number, the number of votes required for approval is rounded up.

Routine Pull Requests
---------------------

Routine pull requests include:

* bug fixes;
* documentation updates;
* tests;
* refactoring that does not substantially alter functionality;
* small feature additions consistent with the existing roadmap.

A routine pull request may be merged after approval by at least two-thirds of the active Maintainers, 
following the voting rules detailed in Section 3. A Maintainer may not approve and merge their own 
pull request, but their vote counts towards the two-thirds consensus.

Major Decisions
---------------

Major decisions require approval by at least two-thirds of the active Maintainers, following the 
voting rules detailed in Section 3. Whether a proposed action constitutes a Major Decision shall 
itself be determined by a vote of the active Maintainers. A proposal shall be treated as a Major 
Decision if at least two-thirds of the active Maintainers determine that it materially affects SCONE's 
architecture, governance, long-term direction, public interfaces, or maintenance practices.

Major Decisions include, but are not limited to:

* significant architectural redesigns;
* major API changes;
* intentional breaking changes;
* adoption of new programming languages or build systems;
* relicensing SCONE;
* creation of official SCONE forks;
* changes to this governance document;
* appointment or removal of Maintainers;
* appointment of Collaborators with write access.

Major decisions should be discussed openly through GitHub Issues, Discussions, or another written 
medium before a vote. If a potential contributor wishes to make a contribution which might be considered 
major, we strongly recommend raising an issue in advance.

Releases
--------

Official releases are coordinated by the active Maintainers. Any Maintainer may prepare a release, but 
official releases should be approved by at least two-thirds of the active Maintainers, following the
voting rules detailed in Section 3.

Repository Administration
-------------------------

Administrative and write access to the GitHub repository should be limited to the Maintainers. Repository
settings that materially affect governance (such as branch protection rules or maintainer permissions)
should not be changed without approval of at least two-thirds of the active Maintainers, following 
the voting rules detailed in Section 3.

Appointment of New Maintainers
------------------------------

New Maintainers may be appointed only by approval of at least two-thirds of the existing active 
Maintainers, following the voting rules detailed in Section 3. Appointment should consider:

* sustained high-quality technical contributions;
* constructive participation in project discussions;
* commitment to long-term maintenance;
* adherence to SCONE's standards.

There is no automatic path from Contributor to Maintainer.

Resignation of Maintainers
--------------------------

A Maintainer may resign at any time. After leaving, a Maintainer will lose repository write and 
administrative access.

Amendments
----------

This governance document may be amended only by approval of at least two-thirds of the active 
Maintainers, following the voting rules detailed in Section 3.

Relationship to Licensing
-------------------------

This document governs project decision-making only. Ownership of source code remains subject to SCONE's
software license and applicable copyright law. Nothing in this document transfers copyright or 
intellectual property between Contributors or Maintainers.
