--REQ 1

SELECT
  PAR_CULT.nom,
  PAR_CEPAGE.nom
FROM
  PAR_CULT,
  PAR_CEPAGE
WHERE
  PAR_CULT.cepage_id = PAR_CEPAGE.id
  AND PAR_CULT.actif = true;

--REQ 2

SELECT
  COUNT(*)
FROM
(
  SELECT id FROM PAR_CEPAGE

  INTERSECT

  SELECT cepage_id FROM PAR_CULT

)
