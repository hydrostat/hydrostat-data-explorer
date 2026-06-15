# Privacy and session-data statement

## Application data model

HydroStat Data Explorer combines read-only bundled data with data temporarily supplied or downloaded during a user's active Shiny session.

The application is designed so that the following remain session-only:

- ANA identifier and password entered to request a token;
- ANA authentication token;
- uploaded hydrological series;
- downloaded discharge, stage and rainfall series;
- partial downloads and resume state;
- analytical products derived from session data;
- download reports, unless the user explicitly downloads them.

The application code does not intentionally write these objects to DuckDB, project files, persistent caches or application-generated logs.

## ANA authentication

Credentials entered in the application are used only to request a token from ANA. After successful authentication, the input fields are cleared. The token is kept in memory for the active session and is discarded when the session ends.

Users must not enter credentials on an untrusted deployment. Public deployment must use HTTPS.

## Hosting platform

The hosting provider may maintain infrastructure, access, security or diagnostic logs according to its own policies. Those platform-level records are outside the application's internal persistence model.

## User downloads

Files explicitly downloaded by the user are saved by the user's browser and are controlled by the user after download.

## Sensitive information

Never submit CPF/CNPJ, passwords, tokens, authorization headers, private hydrological files or confidential reports through public issue trackers.
