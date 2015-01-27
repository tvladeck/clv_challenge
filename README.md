# clv_challange
An assignment to compute a company's CLV based on 175 days of transaction data

Data was given in the following format: 
User ID | Date | Action (Purchase or Sale) | Transaction amount

To compute this, I performed a set of statistical analyses to estimate: 
1. The value of an average customer over a 175 day period and its sensitivity to observable factors such as the size of their first purchase
2. The decay profile of customer engagement, e.g. over what time distribution customers made their next purchase or became inactive
3. And combining (1) & (2) above to estimate lifetime value
