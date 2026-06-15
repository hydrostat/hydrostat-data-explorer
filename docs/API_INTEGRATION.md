# ANA API integration

## Authentication

The application supports user-initiated authentication against ANA HidroWebService. The user's identifier and password are used only to request a token.

The application:

- clears the credential input fields after successful authentication;
- keeps the token only in the active Shiny session;
- does not write credentials or tokens to DuckDB, files, caches or application-generated logs;
- pauses resumable work when authorization expires.

ANA documentation states that tokens have limited validity. Users should avoid unnecessary repeated authentication requests.

## Daily routes used by the current app

```text
/OAUth/v1
/HidroSerieVazao/v1
/HidroSerieCotas/v1
/HidroSerieChuva/v1
```

Downloads are split by year/route. Authorization errors pause the workflow; other failures are retried according to the current session logic. Valid empty responses are recorded as empty rather than as request failures.

## Legacy public service

The application can also read supported HidroWeb files and use the legacy public historical-series service where available. The legacy service is an external dependency and may have different availability or transport behavior from the authenticated API.

## Security boundary

The repository contains no project-author credentials. Public deployments must use HTTPS and must be tested to confirm that hosting logs or error messages do not expose form values, tokens or authorization headers.
