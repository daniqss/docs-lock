import { User, CreateUserRequest } from "../../__generated__/types.gen";
import client from "../client";

export const createUser = (createUserRequest: CreateUserRequest) =>
  client.post("/users", createUserRequest).then((response) => response.data);

export const deleteUser = (userId: string) =>
  client.delete(`/users/${userId}`).then((response) => response.data);

export const getUser = (userId: string) =>
  client.get<User>(`/users/${userId}`).then((response) => response.data);

export const getUsers = async (): Promise<User[]> => {
  const response = await client.get<User[]>("/users");
  return response.data;
};
