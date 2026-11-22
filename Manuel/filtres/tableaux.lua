-- Préserver les petits tableaux de résultats lors de la pagination PDF.
function Table(t)
  if not FORMAT:match('latex') then return nil end
  local rows = #t.head.rows + #t.foot.rows
  for _,body in ipairs(t.bodies) do rows=rows + #body.head + #body.body end
  if rows <= 14 then
    return {pandoc.RawBlock('latex','\\Needspace{' .. tostring(rows+3) .. '\\baselineskip}'),t}
  end
end
