library(salesforcer)
library(dplyr)
library(ggplot2)

sf_auth()
#objects <- sf_list_objects()
head(objects)

CampaignName <- "2025-09-25 Spark"


# WHERE campaign.name like '%Spark%'
# campaign_name__c
# createddate >= 2025-01-01T00:00:00.000z

MemberTable <- sf_query(paste0("SELECT Id, Account_Id__c FROM CampaignMember WHERE Campaign.Name = '",CampaignName,"'"), object_name = "CampaignMember", api_type = "Bulk 2.0", verbose = TRUE)
ConGroupTable <- sf_query("SELECT Id, Account__c, Group_Name__c, Type__c, Parent_Group__c FROM Constituent_Group__c", object_name = "Constituent_Group__c", api_type = "Bulk 2.0", verbose = TRUE)
membercount <- nrow(MemberTable)


FilteredMemberTable <- MemberTable %>%
  filter(!is.na(Account_ID__c))
FilteredConGroupTable <- ConGroupTable %>%
  filter(!is.na(Account__c))

# Will pull in multiple rows if multiple types
JoinedTable <- left_join(FilteredMemberTable, FilteredConGroupTable, by = c("Account_ID__c" = "Account__c"))

# Joining back to original table
FinalJoinedTable <- full_join(MemberTable, JoinedTable, by = c("Id" = "Id.x"))


summary_table <- FinalJoinedTable %>%
  group_by(Type__c, Group_Name__c) %>%
  summarise(
    count = n()
  ) %>%
  mutate(
    percent = round(100 * count / membercount, 2)
  ) %>%
  arrange(Type__c, desc(count))

summary_table$Group_Name__c <- factor(
  summary_table$Group_Name__c,
  levels = summary_table$Group_Name__c
)

print(summary_table)

ggplot(summary_table, aes(x = Group_Name__c, y = count, fill = Type__c)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.5, size = 3, position = position_dodge(width = 0.9)) +
  labs(title = "Count by Group Name and Type", x = "Group Name", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
