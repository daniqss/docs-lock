const { API_URL: API_URL } = import.meta.env

export default function getEnv() {
  return {
    VARIABLES: {
      API_URL,
    },
  }
}
