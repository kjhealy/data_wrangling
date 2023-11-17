
-- Reformat all heading text
function Header(el)
  if el.level == 1 then
      table.insert(el.classes, "inv-title")
      el.attributes["data-background-color"] = "#252F3E"
      return el
  end
end
